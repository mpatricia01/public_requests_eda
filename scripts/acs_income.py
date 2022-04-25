import requests
import csv


# API site: https://www.census.gov/data/developers/data-sets/acs-5year.html
# Variables:https://api.census.gov/data/2019/acs/acs5/variables.html

def main():

    col_names = ['zip_name', 'median_household_income',
                 'medincome_total', 'medincome_under25', 'medincome_2544', 'medincome_4564', 'medincome_65plus',
                 'state_fips_code', 'zip_code']  # last by default
                 # 'cbsa_code']  # last by default

    var_names = ['NAME', 'B19013_001E',
                 'B19049_001E', 'B19049_002E', 'B19049_003E', 'B19049_004E', 'B19049_005E',
                 ]

    save_data('zip code tabulation area', col_names, ','.join(var_names), 'data/acs_income_zip.csv')
    # save_data('metropolitan statistical area/micropolitan statistical area', col_names, ','.join(var_names), 'data/acs_income_metro.csv')


def get_data(region, headings, cols):
    url = 'https://api.census.gov/data/2019/acs/acs5'

    params = {
        'key': 'ab3870e2d20fc643fefe2e23361c8c2d40fbd0e1',
        'for': '{}:*'.format(region),
        'get': cols
    }

    r = requests.get(url=url, params=params)
    
    data = r.json()[1:]
    data.insert(0, headings)

    return data


def save_data(region, headings, cols, file):

    data = get_data(region, headings, cols)

    with open(file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)


if __name__ == '__main__':
    main()
    
