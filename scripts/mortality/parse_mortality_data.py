#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
For each year, parse the pdf manual, then use that information to
unpack the fixed-width data file.

Source data files can be found here:
https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple

Passes basic tests for 2005-2015. Untested on earlier years.
"""

import json
import os
import pandas as pd
import pdb
import re
import tabula  # https://github.com/chezou/tabula-py, requires JDK8 install

from collections import defaultdict, OrderedDict
from slugify import slugify
from string import digits, whitespace


YEARS_TO_PROCESS = [i for i in range(2005, 2016)[::-1]]
INPUT_DATA_DIR = "raw_data"
EXPORT_DATA_DIR = "cleaned_data"
FIELDS_TO_DROP = {'detail_age'}
FIELD_MAP_PATTERNS_TO_DROP = [r'(?i)(_|\s)condition']
MANUALS_DIR = "file_definitions"
GIBBERISH_SEPARATOR = '>!^@^!<'  # should not match anything
UNCODED_FIELDS_TO_KEEP = {r'(?i)ICD_Code.*'}

LAST_PII_YEAR = 2004
ICD_VERSION = {year: 10 for year in range(1999, 2016)}
ICD_VERSION.update({year: 9 for year in range(1979, 1999)})
YEARS_MISSING_ICD_GROUPING = {2003}

DATA_PATHS = {
    2005: 'Mort05uspb.dat',
    2006: 'MORT06.DUSMCPUB',
    2007: 'VS07MORT.DUSMCPUB',
    2008: 'Mort2008us.dat',
    2009: 'VS09MORT.DUSMCPUB',
    2010: 'VS10MORT.DUSMCPUB',
    2011: 'VS11MORT.DUSMCPUB',
    2012: 'VS12MORT.DUSMCPUB',
    2013: 'VS13MORT.DUSMCPUB',
    2014: 'VS14MORT.DUSMCPUB',
    2015: 'VS15MORT.DUSMCPUB',
             }

PDF_PATHS = {
    2005: 'Record_Layout_2005.pdf',
    2006: 'Record_Layout_2006.pdf',
    2007: 'Record_Layout_2007.pdf',
    2008: 'Record_Layout_2008.pdf',
    2009: 'Record_Layout_2009.pdf',
    2010: 'Record_Layout_2010.pdf',
    2011: 'Record_Layout_2011.pdf',
    2012: 'Record_Layout_2012.pdf',
    2013: 'Record_Layout_2013.pdf',
    2014: 'Record_Layout_2014.pdf',
    2015: 'multiple_cause_record_layout_2015.pdf',
                 }

# Detail_Age_Type, Detail_Age are malformed, require special handling
HARDCODED_CODES = {x:
    {'detail_age_type': {1: 'Years', 2: 'Months', 4: 'Days', 5: 'Hours', 6: 'Minutes', 9: pd.np.nan}}
    for x in range(2005, 2016)}

HARDCODED_LOCATIONS = {x: {'detail_age_type': (69, 70), 'detail_age': (70, 73)}
    for x in range(2005, 2016)}


def extract_pdf_section(path, sep=',', pages=None):
    # May log several warnings that can be ignored like:
    # `org.apache.pdfbox.util.PDFStreamEngine processOperator INFO: unsupported/disabled operation`.
    full_path = os.path.join(MANUALS_DIR, path)
    try:
        return tabula.read_pdf(full_path, guess=False, pages=pages,
            pandas_options={'dtype': object, 'header': None, 'sep': sep, 'engine': 'python'})
    except pd.errors.ParserError:
        pdb.set_trace()


def extract_separate_pdf_pages(path):
    # distinct function as passing any sep at all causes an error with multiple_tables
    full_path = os.path.join(MANUALS_DIR, path)
    try:
        return tabula.read_pdf(full_path, multiple_tables=True, guess=False, pages='all',
            pandas_options={'dtype': object, 'header': None})
    except pd.errors.ParserError:
        pdb.set_trace()


def is_column_definition_page(page_data, year):
    # can't just pop the first column definition page; earlier years
    # do actually have different format that won't generate false positive
    clean_page_data = page_data.dropna(how='all').copy()
    cleaned_first_line = str(clean_page_data.iloc[0, -1]).strip()
    starts_with_year = cleaned_first_line.endswith(str(year))
    cleaned_second_line = str(clean_page_data.iloc[1, -1]).strip()
    # spacing is one of the main things tabula fails on
    # have to assume zero or multiple spaces where one normally exists
    follows_with_title = bool(safe_re_search(r'(?i)Mortality\s*Multiple\s*Cause-of-Death\s*Public\s*Use\s*Record$',
        cleaned_second_line))
    cleaned_third_line = str(clean_page_data.iloc[2, -1]).strip()
    is_summary_page = bool(safe_re_search(r'(?i)List\s*of\s*File\s*Data\s*Elements\s*and\s*Tape\s*Locations',
        cleaned_third_line))
    return starts_with_year and follows_with_title and not is_summary_page


def is_icd_ten_group_page(page_data):
    # matches text like 'Tenth Revision 130 Selected Causes of Infant Death Adapted for use by DVS'
    cleaned_first_line = str(page_data.iloc[0, 0]).strip()
    return bool(safe_re_search(r'(?i)Tenth\s*Revision\s*\d{2,3}\s*Selected\s*Causes\s*of\s*(\w+\s*)?Death', cleaned_first_line))


def identify_section_pages(path, year):
    # get page numbers for each section.
    # need 2 passes as multiple_tables does not support the 'sep' option
    column_definition_pages = []
    icd_group_pages = []
    pages = extract_separate_pdf_pages(path)
    have_seen_icd_pages = False
    for page_num, page_data in enumerate(pages):
        if is_column_definition_page(page_data, year):
            column_definition_pages.append(page_num + 1)
        elif is_icd_ten_group_page(page_data):
            icd_group_pages.append(page_num + 1)
            have_seen_icd_pages = True
        elif have_seen_icd_pages:
            # abort after end of icd to avoid pulling territorial column_definition_pages
            break
    return column_definition_pages, icd_group_pages


def extract_pdf(year):
    pdf_path = PDF_PATHS[year]
    column_definition_pages, icd_group_pages = identify_section_pages(pdf_path, year)
    results = [extract_pdf_section(pdf_path, pages=column_definition_pages)]
    # gibberish separator ensures a section is read in as 1 column per page
    results.append(extract_pdf_section(pdf_path, pages=icd_group_pages, sep=GIBBERISH_SEPARATOR))
    return results


def safe_re_search(pattern, text):
    if pd.isnull(text):
        return None
    return re.search(pattern, text)


def is_column_location_row(location_text):
    return bool(safe_re_search(r'^\d{1,5}(-\d{1,5})?', location_text))


def is_encoding_row(code_text):
    return bool(safe_re_search(r'\.{3}.+', code_text))


def is_continuation(field_text):
    return bool(safe_re_search(r'(?i)\s*-\s*Con\.$', field_text))


def is_reserved_position(field_text):
    return bool(safe_re_search(r'(?i)Reserved(\s*Position)?(s)?$', field_text))


def is_condition_entry(field_text):
    return bool(safe_re_search(r'(?i)^\d{1,2}.+\s*Condition$', field_text))


def is_condition_header(field_text):
    return bool(safe_re_search(r'(?i)^\w+\s*-\s*Axis\s*Conditions?$', field_text))


def is_condition_size(field_text):
    return bool(safe_re_search(r'(?i)^Number\s*of\s*\w+\s*-\s*Axis\s*Conditions?$', field_text))


def check_for_duplicate_rows(df):
    duplication_filter = (df.field_or_code.duplicated() & df.is_column_loc)
    if len(df[duplication_filter]) == 0:
        return None
    duplicated_fields = list(df[duplication_filter].field_or_code.unique())
    raise ValueError(f"Duplicate field names: {duplicated_fields}")


def add_basic_column_data(df):
    try:
        df.columns = ['location', 'size', 'field_or_code', 'code_value']
    except ValueError:
        # some pdfs get parsed with only 3 columns. Known issue with 2009 & earlier, exact num unclear
        df.columns = ['location', 'size', 'field_or_code']
        df['code_value'] = df['field_or_code'].apply(lambda x:
            str(x)[str(x).find('...'):].strip() if str(x).find('...') >= 0 else pd.np.nan)
        df['field_or_code'] = df['field_or_code'].apply(lambda x:
            str(x)[:str(x).find('...')].strip() if str(x).find('...') >= 0 else x)

    # tabula errors can introduce quotes. None exist in pdfs checked by hand.
    for column in df.columns:
        df[column] = df[column].apply(lambda x: str(x).strip(whitespace + '"') if not pd.isnull(x) else pd.np.nan)
    df['is_column_loc'] = (df['location'].apply(is_column_location_row) &
        ~df['size'].apply(pd.isnull))
    df['is_encoding'] = (df['code_value'].apply(is_encoding_row) &
        ~df['field_or_code'].apply(pd.isnull) & df['size'].apply(pd.isnull))
    return df


def update_condition_names(df):
    # some condition field names can end up in the code_values column
    # these are migrated over
    malformed_field_filter = (df.is_column_loc & df.field_or_code.isnull())
    df.loc[malformed_field_filter, 'field_or_code'] = df[malformed_field_filter].code_value
    df['condition_entry'] = (df['field_or_code'].apply(is_condition_entry) & df['is_column_loc'])
    df['condition_type'] = (df['field_or_code'].apply(is_condition_header) & df['is_column_loc'])
    df['condition_type'] = df.apply(
        lambda row: row['field_or_code'].split('-')[0].strip().title()
        if row['condition_type'] else None, axis=1)
    df['condition_type'].fillna(method='ffill', inplace=True)
    df = df[~df['field_or_code'].apply(is_condition_header)].copy()
    df['condition_number'] = df.apply(
        lambda row: ''.join([i for i in row['field_or_code'] if i.isnumeric()])
        if row['condition_entry'] else None, axis=1
                                     )
    df.loc[df['condition_entry'], 'field_or_code'] = df[df['condition_entry']].apply(
            lambda row: f'{row["condition_type"]}_condition_{row["condition_number"]}', axis=1)
    df.loc[df['condition_entry'], 'code_value'] = pd.np.nan
    df.loc[df['condition_entry'], 'is_column_loc'] = False
    return df


def drop_useless_rows(df):
    df = df[df.is_column_loc | df.is_encoding].copy()
    df = df[~(df['field_or_code'].apply(is_continuation) & df['is_column_loc'])].copy()
    df = df[~df['field_or_code'].apply(lambda x: str(x).startswith('Reserved'))].copy()
    return df


def handle_missing_values(df):
    df['location'].fillna(method='ffill', inplace=True)
    df['size'].fillna(method='ffill', inplace=True)
    df.fillna(value='', inplace=True)
    return df


def tidy_data(df):
    df['code_value'] = df['code_value'].apply(lambda x:
        str(x)[str(x).find('...') + len('...'):].strip() if str(x).find('...') >= 0 else pd.np.nan)
    # documentation uses 1 based indexing, we need 0 based
    df['location_start'] = df['location'].apply(lambda x: int(x.split('-')[0]) - 1)
    # first page of results has non-unique code keys for US vs territorial datasets
    # retaining first keeps US keys
    df.drop_duplicates(subset={'location', 'size', 'field_or_code'}, inplace=True)
    df['size'] = df['size'].apply(int)
    df['location_end'] = df.apply(
            lambda row: row['location_start'] + row['size'], axis=1)
    df['field'] = df.apply(lambda row: row['field_or_code']
        if any([row['is_column_loc'], row['condition_entry']]) else pd.np.nan, axis=1)
    df['field'].fillna(method='ffill', inplace=True)
    df['field'] = df['field'].apply(lambda x: slugify(x).replace('-', '_'))
    df.rename(columns={'field_or_code': 'code'}, inplace=True)
    return df


def populate_field_data(df, year):
    df['field_to_keep'] = df['field'].apply(lambda x:
        any([bool(safe_re_search(pattern, x)) for pattern in UNCODED_FIELDS_TO_KEEP]))
    df = df[~df['is_column_loc'] | df['field_to_keep']].copy()
    df = df[~df['field'].isin(FIELDS_TO_DROP)].copy()
    # enforcing title case to remove source of inconsistencies across years
    df['code'] = df['code'].apply(str.title)
    field_codes = defaultdict(dict)
    df.apply(lambda row:
        field_codes[row['field']].update({row['code']: row['code_value']}), axis=1)
    field_codes.update(HARDCODED_CODES[year])
    field_codes = {k: v for k, v in field_codes.items()
        if not any([re.search(pattern, k) for pattern in FIELD_MAP_PATTERNS_TO_DROP])}
    df.drop_duplicates(inplace=True)
    df.sort_values(by=['location_start'], inplace=True)
    df['location'] = df.apply(
        lambda row: (row['location_start'], row['location_end']), axis=1)
    df.drop_duplicates(subset=['location'], inplace=True)
    field_locations = OrderedDict()
    df.apply(lambda row: field_locations.update({row['field']: row['location']}), axis=1)
    field_locations.update(HARDCODED_LOCATIONS[year])
    field_locations = OrderedDict(sorted(field_locations.items(), key=lambda x: x[1][0]))
    return field_locations, dict(field_codes), df


def parse_pdf_data(df, year):
    df = add_basic_column_data(df)
    df = drop_useless_rows(df)
    df = update_condition_names(df)
    check_for_duplicate_rows(df)
    df = handle_missing_values(df)
    df = tidy_data(df)
    return populate_field_data(df, year)


def read_dataset(year, field_locations):
    full_path = os.path.join(INPUT_DATA_DIR, DATA_PATHS[year])
    return pd.read_fwf(full_path, colspecs=list(field_locations.values()),
        names=list(field_locations.keys()), header=None, dtype=object)


def validate_data_types(df, year):
    # since we don't know the complete list of columns to expect, we only
    # validate a small sample that definitely ought exist with consistent formats
    if not all([
        set(df['autopsy'].apply(str.upper).unique()) == {'Y', 'N', 'U'},
        set(df['sex'].unique()) == {'M', 'F'},
        all(df['detail_age_type'].apply(str.isnumeric)),
        df['current_data_year'].unique().tolist() == [str(year)],
        len([x for x in df.columns if 'record' in x]) >= 20,
        len([x for x in df.columns if 'entity' in x]) >= 20,
               ]):
        pdb.set_trace()
        raise ValueError(f"Invalid datatype found in {year} data")


def safe_first_re_match(pattern, text):
    match = safe_re_search(pattern, text)
    if match:
        return match[0]


def extract_cause_title(text):
    return safe_first_re_match(r'(?i)\b\d{1,4}\s*selected\s*causes\b', text)


def is_idc_data_header(text):
    return bool(safe_re_search(r'(?i)^recodetsexagecause', re.sub('\s', '', text)))


def extract_recode(text):
    return safe_first_re_match(r'^\d{3}', text)


def remove_filler_rows(df):
    df['is_header'] = df['text'].apply(is_idc_data_header)
    cause_title_indexes = list(df[df.is_cause_title].index)
    header_indexes = list(df[df.is_header].index)
    filler_indexes = [x for x in zip(cause_title_indexes, header_indexes)]
    for min_idx, max_idx in filler_indexes:
        df = df[(df.index < min_idx) | (df.index > max_idx)].copy()
    return df


def validate_ICD_codes(icd_codes):
    # codes should be form complete sequence within each group
    for cause_title, code_group in icd_codes.items():
        int_icd_codes = sorted([int(x) for x in code_group.keys()])
        expected_code_range = [i for i in range(min(int_icd_codes), max(int_icd_codes) + 1)]
        if int_icd_codes != expected_code_range:
            pdb.set_trace()
            raise ValueError(f'ICD codes for {cause_title} do not form complete sequence')


def read_ICD_codes(df):
    df.columns = ['text']
    df.dropna(inplace=True)
    # some lines end up improperly quote wrapped due to tabula error
    df['text'] = df[~df['text'].isnull()].copy()['text'].apply(
        lambda x: x.strip('"') if not pd.isnull(x) else pd.np.nan)
    df['cause_title'] = df['text'].apply(extract_cause_title)
    df['is_cause_title'] = ~df['cause_title'].isnull()
    df['cause_title'].fillna(method='ffill', inplace=True)
    df = remove_filler_rows(df)
    df['recode'] = df['text'].apply(extract_recode)
    df['recode'].fillna(method='ffill', inplace=True)
    # very first row can still be blank; can be safely dropped
    df = df.dropna(subset=['text', 'recode'], how='any').copy()
    df['text'] = (df[['text', 'recode', 'cause_title']].groupby(['cause_title', 'recode'])
        .transform(lambda x: ' '.join(x)))
    df.drop_duplicates(inplace=True)
    # tabula fails to retain spacing; we cannot recover subtotal/gender/age codes
    df['text'] = df['text'].apply(lambda x: x.lstrip(digits + whitespace).strip())
    icd_codes = {title: dict() for title in df['cause_title'].unique()}
    df.apply(lambda row: icd_codes[row['cause_title']].update({row['recode']: row['text']}), axis=1)
    validate_ICD_codes(icd_codes)
    return icd_codes


def update_field_map(field_map, icd_codes):
    for code_group, icd_values in icd_codes.items():
        group_number = safe_first_re_match('\d{1,4}', code_group)
        matching_pattern = f'(?i){group_number}.+Cause_Recode$'
        field_map_matching_key = [x for x in field_map.keys() if safe_re_search(matching_pattern, x)][0]
        field_map[field_map_matching_key] = icd_values
    return field_map


def export_code_maps(field_map, year):
    with open(os.path.join(EXPORT_DATA_DIR, f'{year}_codes.json'), 'w+') as f_open:
        json.dump(field_map, f_open)


def process_year(year):
    print(f"Unpacking pdf for {year}")
    documentation_data = extract_pdf(year)
    field_locations, field_map, df = parse_pdf_data(documentation_data[0], year)
    print(f"Parsing fixed with file for {year}")
    df = read_dataset(year, field_locations)
    validate_data_types(df, year)
    df.to_csv(os.path.join(EXPORT_DATA_DIR, f'{year}_data.csv'), index=False)
    print(f"Exporting column code mappings")
    icd_codes = read_ICD_codes(documentation_data[1])
    field_map = update_field_map(field_map, icd_codes)
    export_code_maps(field_map, year)
    print(f"Finished {year}")


def process_all_years():
    for year in YEARS_TO_PROCESS:
        process_year(year)


if __name__ == '__main__':
    process_all_years()
