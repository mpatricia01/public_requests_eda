import csv


def main():

    zip_list = {}

    with open('./data/zip_tract_12-19.csv', mode='r', encoding='utf-8-sig') as infile:
        reader = csv.DictReader(infile)
        for row in reader:

            if row['ZIP'] not in zip_list:
                zip_list[row['ZIP']] = []

            zip_list[row['ZIP']].append((row['TRACT'], float(row['RES_RATIO'])))

    fieldnames = ['zip_code']
    
    for i in range(1, 62):
        fieldnames.extend([f'tract_{i}', f'tract_{i}_ratio'])
    
    with open('./data/zip_code_tract.csv', 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()

        for zip_code, tract_list in zip_list.items():

            # check that res_ratio of all tract's in zip adds up to 1
            res_ratio_total = sum([i[1] for i in tract_list])
            if not (0.99 < res_ratio_total < 1.01) and len(tract_list) > 1:
                # could be 0 if no res pop in tract (ie. all business or other) - but use tract anyway if it's the only one
                # exception: will be arbitrary in below cases
                # 0.0 = 98068 [('42660', 0.0), ('21260', 0.0)]
                # 0.0 = 36849 [('10760', 0.0), ('12220', 0.0)]
                print(res_ratio_total, '=', zip_code, tract_list)

            ordered_tract_list = sorted(tract_list, key=lambda x: x[1], reverse=True)

            row = {
                'zip_code': zip_code
            }

            n = 1
            curr_ratio = 1
            for tract_code, tract_ratio in ordered_tract_list:

                # check tract's are in order by res_ratio
                if tract_ratio > curr_ratio:
                    print(ordered_tract_list)

                row['tract_{}'.format(n)] = tract_code[-6:]
                row['tract_{}_ratio'.format(n)] = tract_ratio

                n += 1
                curr_ratio = tract_ratio

            writer.writerow(row)


if __name__ == '__main__':
    main()
