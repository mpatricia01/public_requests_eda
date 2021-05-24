import requests
import json
import csv
import os


'''
program to get zip code ACS data
API site: https://www.census.gov/data/developers/data-sets/acs-5year.html
Variables:https://api.census.gov/data/2019/acs/acs5/variables.html
'''

def main():

    # MSA data

    col_names = ['msa_name', 'pop_total', 'pop_total_25plus', 'median_household_income',
                 'pop_white_m_15-17', 'pop_white_m_18-19', 'pop_white_f_15-17', 'pop_white_f_18-19',
                 'pop_black_m_15-17', 'pop_black_m_18-19', 'pop_black_f_15-17', 'pop_black_f_18-19',
                 'pop_amerindian_m_15-17', 'pop_amerindian_m_18-19', 'pop_amerindian_f_15-17', 'pop_amerindian_f_18-19',
                 'pop_asian_m_15-17', 'pop_asian_m_18-19', 'pop_asian_f_15-17', 'pop_asian_f_18-19',
                 'pop_nativehawaii_m_15-17', 'pop_nativehawaii_m_18-19', 'pop_nativehawaii_f_15-17', 'pop_nativehawaii_f_18-19',
                 'pop_otherrace_m_15-17', 'pop_otherrace_m_18-19', 'pop_otherrace_f_15-17', 'pop_otherrace_f_18-19',
                 'pop_tworaces_m_15-17', 'pop_tworaces_m_18-19', 'pop_tworaces_f_15-17', 'pop_tworaces_f_18-19',
                 'pop_hispanic_m_15-17', 'pop_hispanic_m_18-19', 'pop_hispanic_f_15-17', 'pop_hispanic_f_18-19',
                 'pop_white', 'pop_black', 'pop_amerindian', 'pop_asian',
                 'pop_nativehawaii', 'pop_otherrace', 'pop_tworaces', 'pop_hispanic',
                 #'pop_edu_attain_doct', 'pop_edu_attain_prof', 'pop_edu_attain_master', 'pop_edu_attain_bach',
                 #'pop_edu_attain_assoc', 'pop_edu_somecollege_1plusyrs', 'pop_edu_somecollege_under1yr', 'pop_edu_GED', 'pop_edu_hs',
                 'zipcode']  # last by default

    var_names = ['NAME', 'B01003_001E', 'B15003_001E', 'B19013_001E',
                 'B01001H_006E', 'B01001H_007E', 'B01001H_021E', 'B01001H_022E',
                 'B01001B_006E', 'B01001B_007E', 'B01001B_021E', 'B01001B_022E',
                 'B01001C_006E', 'B01001C_007E', 'B01001C_021E', 'B01001C_022E',
                 'B01001D_006E', 'B01001D_007E', 'B01001D_021E', 'B01001D_022E',
                 'B01001E_006E', 'B01001E_007E', 'B01001E_021E', 'B01001E_022E',
                 'B01001F_006E', 'B01001F_007E', 'B01001F_021E', 'B01001F_022E',
                 'B01001G_006E', 'B01001G_007E', 'B01001G_021E', 'B01001G_022E',
                 'B01001I_006E', 'B01001I_007E', 'B01001I_021E', 'B01001I_022E',
                 'B03002_003E', 'B03002_004E', 'B03002_005E', 'B03002_006E',
                 'B03002_007E', 'B03002_008E', 'B03002_009E', 'B03002_012E',
                 #'B15003_025E', 'B15003_024E', 'B15003_023E', 'B15003_022E',
                 #'B15003_021E', 'B15003_020E', 'B15003_019E', 'B15003_018E',
                 'B15003_017E',
                 ]

    save_data('zip code tabulation area', col_names, ','.join(var_names), 'msa_raw.csv')



def get_data(region, headings, cols):
    url = 'https://api.census.gov/data/2019/acs/acs5'

    params = {
        'key': 'ab3870e2d20fc643fefe2e23361c8c2d40fbd0e1',
        'for': '{}:*'.format(region),
        'get': cols
    }

    r = requests.get(url=url, params=params)

    data = json.loads(r.text)[1:]
    data.insert(0, headings)

    return data


def save_data(region, headings, cols, file):

    data = get_data(region, headings, cols)

    os.chdir("../data")
    
    with open(file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)


if __name__ == '__main__':
    main()
