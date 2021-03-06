context('External validity of regex')

iso3c_of <- function(name) countrycode(name, 'country.name', 'iso3c', warn = TRUE)

test_that('a variety of country names yield correct iso3c codes', {
    expect_equal(iso3c_of('Aruba'), 'ABW')
    expect_equal(iso3c_of('Afghanistan'), 'AFG')
    expect_equal(iso3c_of('Angola'), 'AGO')
    expect_equal(iso3c_of('Anguilla'), 'AIA')
    expect_equal(iso3c_of('åland Islands'), 'ALA')
    expect_equal(iso3c_of('Albania'), 'ALB')
    expect_equal(iso3c_of('Andorra'), 'AND')
    expect_equal(iso3c_of('United Arab Emirates'), 'ARE')
    expect_equal(iso3c_of('Argentina'), 'ARG')
    expect_equal(iso3c_of('Armenia'), 'ARM')
    expect_equal(iso3c_of('American Samoa'), 'ASM')
    expect_equal(iso3c_of('Antarctica'), 'ATA')
    expect_equal(iso3c_of('French Southern and Antarctic Lands'), 'ATF')
    expect_equal(iso3c_of('Antigua and Barbuda'), 'ATG')
    expect_equal(iso3c_of('Australia'), 'AUS')
    expect_equal(iso3c_of('Austria'), 'AUT')
    expect_equal(iso3c_of('Azerbaijan'), 'AZE')
    expect_equal(iso3c_of('Burundi'), 'BDI')
    expect_equal(iso3c_of('Belgium'), 'BEL')
    expect_equal(iso3c_of('Benin'), 'BEN')
    expect_equal(iso3c_of('Caribbean Netherlands'), 'BES')
    expect_equal(iso3c_of('Burkina Faso'), 'BFA')
    expect_equal(iso3c_of('Bangladesh'), 'BGD')
    expect_equal(iso3c_of('Bulgaria'), 'BGR')
    expect_equal(iso3c_of('Bahrain'), 'BHR')
    expect_equal(iso3c_of('The Bahamas'), 'BHS')
    expect_equal(iso3c_of('Bosnia and Herzegovina'), 'BIH')
    expect_equal(iso3c_of('Saint Barth\u00E9lemy'), 'BLM')
    expect_equal(iso3c_of('Belarus'), 'BLR')
    expect_equal(iso3c_of('Belize'), 'BLZ')
    expect_equal(iso3c_of('Bermuda'), 'BMU')
    expect_equal(iso3c_of('Bolivia'), 'BOL')
    expect_equal(iso3c_of('Brazil'), 'BRA')
    expect_equal(iso3c_of('Barbados'), 'BRB')
    expect_equal(iso3c_of('Brunei'), 'BRN')
    expect_equal(iso3c_of('Bhutan'), 'BTN')
    expect_equal(iso3c_of('Bouvet Island'), 'BVT')
    expect_equal(iso3c_of('Botswana'), 'BWA')
    expect_equal(iso3c_of('Central African Republic'), 'CAF')
    expect_equal(iso3c_of('Canada'), 'CAN')
    expect_equal(iso3c_of('Cocos (Keeling) Islands'), 'CCK')
    expect_equal(iso3c_of('Switzerland'), 'CHE')
    expect_equal(iso3c_of('Chile'), 'CHL')
    expect_equal(iso3c_of('China'), 'CHN')
    expect_equal(iso3c_of('C\u00F4te d\'Ivoire'), 'CIV')
    expect_equal(iso3c_of('Cameroon'), 'CMR')
    expect_equal(iso3c_of('Democratic Republic of the Congo'), 'COD')
    expect_equal(iso3c_of('Republic of the Congo'), 'COG')
    expect_equal(iso3c_of('Cook Islands'), 'COK')
    expect_equal(iso3c_of('Colombia'), 'COL')
    expect_equal(iso3c_of('Comoros'), 'COM')
    expect_equal(iso3c_of('Cabo Verde'), 'CPV')
    expect_equal(iso3c_of('Costa Rica'), 'CRI')
    expect_equal(iso3c_of('Cuba'), 'CUB')
    expect_equal(iso3c_of('Cura\u00E7ao'), 'CUW')
    expect_equal(iso3c_of('Christmas Island'), 'CXR')
    expect_equal(iso3c_of('Cayman Islands'), 'CYM')
    expect_equal(iso3c_of('Cyprus'), 'CYP')
    expect_equal(iso3c_of('Czech Republic'), 'CZE')
    expect_equal(iso3c_of('Germany'), 'DEU')
    expect_equal(iso3c_of('Djibouti'), 'DJI')
    expect_equal(iso3c_of('Dominica'), 'DMA')
    expect_equal(iso3c_of('Denmark'), 'DNK')
    expect_equal(iso3c_of('Dominican Republic'), 'DOM')
    expect_equal(iso3c_of('Algeria'), 'DZA')
    expect_equal(iso3c_of('Ecuador'), 'ECU')
    expect_equal(iso3c_of('Egypt'), 'EGY')
    expect_equal(iso3c_of('Eritrea'), 'ERI')
    expect_equal(iso3c_of('Western Sahara'), 'ESH')
    expect_equal(iso3c_of('Spain'), 'ESP')
    expect_equal(iso3c_of('Estonia'), 'EST')
    expect_equal(iso3c_of('Ethiopia'), 'ETH')
    expect_equal(iso3c_of('Finland'), 'FIN')
    expect_equal(iso3c_of('Fiji'), 'FJI')
    expect_equal(iso3c_of('Falkland Islands'), 'FLK')
    expect_equal(iso3c_of('France'), 'FRA')
    expect_equal(iso3c_of('Faroe Islands'), 'FRO')
    expect_equal(iso3c_of('Federated States of Micronesia'), 'FSM')
    expect_equal(iso3c_of('Gabon'), 'GAB')
    expect_equal(iso3c_of('United Kingdom'), 'GBR')
    expect_equal(iso3c_of('Georgia'), 'GEO')
    expect_equal(iso3c_of('Guernsey'), 'GGY')
    expect_equal(iso3c_of('Ghana'), 'GHA')
    expect_equal(iso3c_of('Gibraltar'), 'GIB')
    expect_equal(iso3c_of('Guinea'), 'GIN')
    expect_equal(iso3c_of('Guadeloupe'), 'GLP')
    expect_equal(iso3c_of('The Gambia'), 'GMB')
    expect_equal(iso3c_of('Guinea-Bissau'), 'GNB')
    expect_equal(iso3c_of('Equatorial Guinea'), 'GNQ')
    expect_equal(iso3c_of('Greece'), 'GRC')
    expect_equal(iso3c_of('Grenada'), 'GRD')
    expect_equal(iso3c_of('Greenland'), 'GRL')
    expect_equal(iso3c_of('Guatemala'), 'GTM')
    expect_equal(iso3c_of('French Guiana'), 'GUF')
    expect_equal(iso3c_of('Guam'), 'GUM')
    expect_equal(iso3c_of('Guyana'), 'GUY')
    expect_equal(iso3c_of('Hong Kong'), 'HKG')
    expect_equal(iso3c_of('Heard Island and McDonald Islands'), 'HMD')
    expect_equal(iso3c_of('Honduras'), 'HND')
    expect_equal(iso3c_of('Croatia'), 'HRV')
    expect_equal(iso3c_of('Haiti'), 'HTI')
    expect_equal(iso3c_of('Hungary'), 'HUN')
    expect_equal(iso3c_of('Indonesia'), 'IDN')
    expect_equal(iso3c_of('Isle of Man'), 'IMN')
    expect_equal(iso3c_of('India'), 'IND')
    expect_equal(iso3c_of('British Indian Ocean Territory'), 'IOT')
    expect_equal(iso3c_of('Republic of Ireland'), 'IRL')
    expect_equal(iso3c_of('Iran'), 'IRN')
    expect_equal(iso3c_of('Iraq'), 'IRQ')
    expect_equal(iso3c_of('Iceland'), 'ISL')
    expect_equal(iso3c_of('Israel'), 'ISR')
    expect_equal(iso3c_of('Italy'), 'ITA')
    expect_equal(iso3c_of('Jamaica'), 'JAM')
    expect_equal(iso3c_of('Jersey'), 'JEY')
    expect_equal(iso3c_of('Jordan'), 'JOR')
    expect_equal(iso3c_of('Japan'), 'JPN')
    expect_equal(iso3c_of('Kazakhstan'), 'KAZ')
    expect_equal(iso3c_of('Kenya'), 'KEN')
    expect_equal(iso3c_of('Kyrgyzstan'), 'KGZ')
    expect_equal(iso3c_of('Cambodia'), 'KHM')
    expect_equal(iso3c_of('Kiribati'), 'KIR')
    expect_equal(iso3c_of('Saint Kitts and Nevis'), 'KNA')
    expect_equal(iso3c_of('South Korea'), 'KOR')
    expect_equal(iso3c_of('Kuwait'), 'KWT')
    expect_equal(iso3c_of('Laos'), 'LAO')
    expect_equal(iso3c_of('Lebanon'), 'LBN')
    expect_equal(iso3c_of('Liberia'), 'LBR')
    expect_equal(iso3c_of('Libya'), 'LBY')
    expect_equal(iso3c_of('Saint Lucia'), 'LCA')
    expect_equal(iso3c_of('Liechtenstein'), 'LIE')
    expect_equal(iso3c_of('Sri Lanka'), 'LKA')
    expect_equal(iso3c_of('Lesotho'), 'LSO')
    expect_equal(iso3c_of('Lithuania'), 'LTU')
    expect_equal(iso3c_of('Luxembourg'), 'LUX')
    expect_equal(iso3c_of('Latvia'), 'LVA')
    expect_equal(iso3c_of('Macau'), 'MAC')
    expect_equal(iso3c_of('Collectivity of Saint Martin'), 'MAF')
    expect_equal(iso3c_of('Morocco'), 'MAR')
    expect_equal(iso3c_of('Monaco'), 'MCO')
    expect_equal(iso3c_of('Moldova'), 'MDA')
    expect_equal(iso3c_of('Madagascar'), 'MDG')
    expect_equal(iso3c_of('Maldives'), 'MDV')
    expect_equal(iso3c_of('Mexico'), 'MEX')
    expect_equal(iso3c_of('Marshall Islands'), 'MHL')
    expect_equal(iso3c_of('Republic of Macedonia'), 'MKD')
    expect_equal(iso3c_of('Mali'), 'MLI')
    expect_equal(iso3c_of('Malta'), 'MLT')
    expect_equal(iso3c_of('Myanmar'), 'MMR')
    expect_equal(iso3c_of('Montenegro'), 'MNE')
    expect_equal(iso3c_of('Mongolia'), 'MNG')
    expect_equal(iso3c_of('Northern Mariana Islands'), 'MNP')
    expect_equal(iso3c_of('Mozambique'), 'MOZ')
    expect_equal(iso3c_of('Mauritania'), 'MRT')
    expect_equal(iso3c_of('Montserrat'), 'MSR')
    expect_equal(iso3c_of('Martinique'), 'MTQ')
    expect_equal(iso3c_of('Mauritius'), 'MUS')
    expect_equal(iso3c_of('Malawi'), 'MWI')
    expect_equal(iso3c_of('Malaysia'), 'MYS')
    expect_equal(iso3c_of('Mayotte'), 'MYT')
    expect_equal(iso3c_of('Namibia'), 'NAM')
    expect_equal(iso3c_of('New Caledonia'), 'NCL')
    expect_equal(iso3c_of('Niger'), 'NER')
    expect_equal(iso3c_of('Norfolk Island'), 'NFK')
    expect_equal(iso3c_of('Nigeria'), 'NGA')
    expect_equal(iso3c_of('Nicaragua'), 'NIC')
    expect_equal(iso3c_of('Niue'), 'NIU')
    expect_equal(iso3c_of('Netherlands'), 'NLD')
    expect_equal(iso3c_of('Norway'), 'NOR')
    expect_equal(iso3c_of('Nepal'), 'NPL')
    expect_equal(iso3c_of('Nauru'), 'NRU')
    expect_equal(iso3c_of('New Zealand'), 'NZL')
    expect_equal(iso3c_of('Oman'), 'OMN')
    expect_equal(iso3c_of('Pakistan'), 'PAK')
    expect_equal(iso3c_of('Panama'), 'PAN')
    expect_equal(iso3c_of('Pitcairn Islands'), 'PCN')
    expect_equal(iso3c_of('Peru'), 'PER')
    expect_equal(iso3c_of('Philippines'), 'PHL')
    expect_equal(iso3c_of('Palau'), 'PLW')
    expect_equal(iso3c_of('Papua New Guinea'), 'PNG')
    expect_equal(iso3c_of('Poland'), 'POL')
    expect_equal(iso3c_of('Puerto Rico'), 'PRI')
    expect_equal(iso3c_of('North Korea'), 'PRK')
    expect_equal(iso3c_of('Portugal'), 'PRT')
    expect_equal(iso3c_of('Paraguay'), 'PRY')
    expect_equal(iso3c_of('State of Palestine'), 'PSE')
    expect_equal(iso3c_of('French Polynesia'), 'PYF')
    expect_equal(iso3c_of('Qatar'), 'QAT')
    expect_equal(iso3c_of('R\u00E9union'), 'REU')
    expect_equal(iso3c_of('Romania'), 'ROU')
    expect_equal(iso3c_of('Russia'), 'RUS')
    expect_equal(iso3c_of('Rwanda'), 'RWA')
    expect_equal(iso3c_of('Saudi Arabia'), 'SAU')
    expect_equal(iso3c_of('Sudan'), 'SDN')
    expect_equal(iso3c_of('Senegal'), 'SEN')
    expect_equal(iso3c_of('Singapore'), 'SGP')
    expect_equal(iso3c_of('South Georgia and the South Sandwich Islands'), 'SGS')
    expect_equal(iso3c_of('Saint Helena, Ascension and Tristan da Cunha'), 'SHN')
    expect_equal(iso3c_of('Svalbard and Jan Mayen'), 'SJM')
    expect_equal(iso3c_of('Solomon Islands'), 'SLB')
    expect_equal(iso3c_of('Sierra Leone'), 'SLE')
    expect_equal(iso3c_of('El Salvador'), 'SLV')
    expect_equal(iso3c_of('San Marino'), 'SMR')
    expect_equal(iso3c_of('Somalia'), 'SOM')
    expect_equal(iso3c_of('Saint Pierre and Miquelon'), 'SPM')
    expect_equal(iso3c_of('Serbia'), 'SRB')
    expect_equal(iso3c_of('South Sudan'), 'SSD')
    expect_equal(iso3c_of('S\u00E3o Tom\u00E9 and Pr\u00EDncipe'), 'STP')
    expect_equal(iso3c_of('Suriname'), 'SUR')
    expect_equal(iso3c_of('Slovakia'), 'SVK')
    expect_equal(iso3c_of('Slovenia'), 'SVN')
    expect_equal(iso3c_of('Sweden'), 'SWE')
    expect_equal(iso3c_of('Swaziland'), 'SWZ')
    expect_equal(iso3c_of('Sint Maarten'), 'SXM')
    expect_equal(iso3c_of('Seychelles'), 'SYC')
    expect_equal(iso3c_of('Syria'), 'SYR')
    expect_equal(iso3c_of('Turks and Caicos Islands'), 'TCA')
    expect_equal(iso3c_of('Chad'), 'TCD')
    expect_equal(iso3c_of('Togo'), 'TGO')
    expect_equal(iso3c_of('Thailand'), 'THA')
    expect_equal(iso3c_of('Tajikistan'), 'TJK')
    expect_equal(iso3c_of('Tokelau'), 'TKL')
    expect_equal(iso3c_of('Turkmenistan'), 'TKM')
    expect_equal(iso3c_of('East Timor'), 'TLS')
    expect_equal(iso3c_of('Tonga'), 'TON')
    expect_equal(iso3c_of('Trinidad and Tobago'), 'TTO')
    expect_equal(iso3c_of('Tunisia'), 'TUN')
    expect_equal(iso3c_of('Turkey'), 'TUR')
    expect_equal(iso3c_of('Tuvalu'), 'TUV')
    expect_equal(iso3c_of('Taiwan'), 'TWN')
    expect_equal(iso3c_of('Tanzania'), 'TZA')
    expect_equal(iso3c_of('Uganda'), 'UGA')
    expect_equal(iso3c_of('Ukraine'), 'UKR')
    expect_equal(iso3c_of('United States Minor Outlying Islands'), 'UMI')
    expect_equal(iso3c_of('Uruguay'), 'URY')
    expect_equal(iso3c_of('United States'), 'USA')
    expect_equal(iso3c_of('Uzbekistan'), 'UZB')
    expect_equal(iso3c_of('Vatican City'), 'VAT')
    expect_equal(iso3c_of('Saint Vincent and the Grenadines'), 'VCT')
    expect_equal(iso3c_of('Venezuela'), 'VEN')
    expect_equal(iso3c_of('British Virgin Islands'), 'VGB')
    expect_equal(iso3c_of('United States Virgin Islands'), 'VIR')
    expect_equal(iso3c_of('Viet Nam'), 'VNM')
    expect_equal(iso3c_of('Vanuatu'), 'VUT')
    expect_equal(iso3c_of('Wallis and Futuna'), 'WLF')
    expect_equal(iso3c_of('Samoa'), 'WSM')
    expect_equal(iso3c_of('Yemen'), 'YEM')
    expect_equal(iso3c_of('South Africa'), 'ZAF')
    expect_equal(iso3c_of('Zambia'), 'ZMB')
    expect_equal(iso3c_of('Zimbabwe'), 'ZWE')
})
