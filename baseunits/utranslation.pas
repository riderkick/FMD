{ Simple Translation Collector

  Copyright (C) 2015 Nur Cholif

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit uTranslation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil, LCLTranslator, Translations;

type
  TPoLanguage = record
    id: String;
    name: String;
  end;

  TPoLanguages = array of TPoLanguage;

const
  Lang_english: array[0..184] of array[0..1] of string = (
    ('ab', 'Abkhaz'),
    ('aa', 'Afar'),
    ('af', 'Afrikaans'),
    ('ak', 'Akan'),
    ('sq', 'Albanian'),
    ('am', 'Amharic'),
    ('ar', 'Arabic'),
    ('an', 'Aragonese'),
    ('hy', 'Armenian'),
    ('as', 'Assamese'),
    ('av', 'Avaric'),
    ('ae', 'Avestan'),
    ('ay', 'Aymara'),
    ('az', 'Azerbaijani'),
    ('bm', 'Bambara'),
    ('ba', 'Bashkir'),
    ('eu', 'Basque'),
    ('be', 'Belarusian'),
    ('bn', 'Bengali, Bangla'),
    ('bh', 'Bihari'),
    ('bi', 'Bislama'),
    ('bs', 'Bosnian'),
    ('br', 'Breton'),
    ('bg', 'Bulgarian'),
    ('my', 'Burmese'),
    ('ca', 'Catalan'),
    ('ch', 'Chamorro'),
    ('ce', 'Chechen'),
    ('ny', 'Chichewa, Chewa, Nyanja'),
    ('zh', 'Chinese'),
    ('cv', 'Chuvash'),
    ('kw', 'Cornish'),
    ('co', 'Corsican'),
    ('cr', 'Cree'),
    ('hr', 'Croatian'),
    ('cs', 'Czech'),
    ('da', 'Danish'),
    ('dv', 'Divehi, Dhivehi, Maldivian'),
    ('nl', 'Dutch'),
    ('dz', 'Dzongkha'),
    ('en', 'English'),
    ('eo', 'Esperanto'),
    ('et', 'Estonian'),
    ('ee', 'Ewe'),
    ('fo', 'Faroese'),
    ('fj', 'Fijian'),
    ('fi', 'Finnish'),
    ('fr', 'French'),
    ('ff', 'Fula, Fulah, Pulaar, Pular'),
    ('gl', 'Galician'),
    ('ka', 'Georgian'),
    ('de', 'German'),
    ('el', 'Greek (modern)'),
    ('gn', 'Guaraní'),
    ('gu', 'Gujarati'),
    ('ht', 'Haitian, Haitian Creole'),
    ('ha', 'Hausa'),
    ('he', 'Hebrew (modern)'),
    ('hz', 'Herero'),
    ('hi', 'Hindi'),
    ('ho', 'Hiri Motu'),
    ('hu', 'Hungarian'),
    ('ia', 'Interlingua'),
    ('id', 'Indonesian'),
    ('ie', 'Interlingue'),
    ('ga', 'Irish'),
    ('ig', 'Igbo'),
    ('ik', 'Inupiaq'),
    ('io', 'Ido'),
    ('is', 'Icelandic'),
    ('it', 'Italian'),
    ('iu', 'Inuktitut'),
    ('ja', 'Japanese'),
    ('jv', 'Javanese'),
    ('kl', 'Kalaallisut, Greenlandic'),
    ('kn', 'Kannada'),
    ('kr', 'Kanuri'),
    ('ks', 'Kashmiri'),
    ('kk', 'Kazakh'),
    ('km', 'Khmer'),
    ('ki', 'Kikuyu, Gikuyu'),
    ('rw', 'Kinyarwanda'),
    ('ky', 'Kyrgyz'),
    ('kv', 'Komi'),
    ('kg', 'Kongo'),
    ('ko', 'Korean'),
    ('ku', 'Kurdish'),
    ('kj', 'Kwanyama, Kuanyama'),
    ('la', 'Latin'),
    ('lld', 'Ladin'),
    ('lb', 'Luxembourgish, Letzeburgesch'),
    ('lg', 'Ganda'),
    ('li', 'Limburgish, Limburgan, Limburger'),
    ('ln', 'Lingala'),
    ('lo', 'Lao'),
    ('lt', 'Lithuanian'),
    ('lu', 'Luba-Katanga'),
    ('lv', 'Latvian'),
    ('gv', 'Manx'),
    ('mk', 'Macedonian'),
    ('mg', 'Malagasy'),
    ('ms', 'Malay'),
    ('ml', 'Malayalam'),
    ('mt', 'Maltese'),
    ('mi', 'Māori'),
    ('mr', 'Marathi (Marāṭhī)'),
    ('mh', 'Marshallese'),
    ('mn', 'Mongolian'),
    ('na', 'Nauru'),
    ('nv', 'Navajo, Navaho'),
    ('nd', 'Northern Ndebele'),
    ('ne', 'Nepali'),
    ('ng', 'Ndonga'),
    ('nb', 'Norwegian Bokmål'),
    ('nn', 'Norwegian Nynorsk'),
    ('no', 'Norwegian'),
    ('ii', 'Nuosu'),
    ('nr', 'Southern Ndebele'),
    ('oc', 'Occitan'),
    ('oj', 'Ojibwe, Ojibwa'),
    ('cu', 'Old Church Slavonic, Church Slavonic, Old Bulgarian'),
    ('om', 'Oromo'),
    ('or', 'Oriya'),
    ('os', 'Ossetian, Ossetic'),
    ('pa', 'Panjabi, Punjabi'),
    ('pi', 'Pāli'),
    ('fa', 'Persian (Farsi)'),
    ('pl', 'Polish'),
    ('ps', 'Pashto, Pushto'),
    ('pt', 'Portuguese'),
    ('qu', 'Quechua'),
    ('rm', 'Romansh'),
    ('rn', 'Kirundi'),
    ('ro', 'Romanian'),
    ('ru', 'Russian'),
    ('sa', 'Sanskrit (Saṁskṛta)'),
    ('sc', 'Sardinian'),
    ('sd', 'Sindhi'),
    ('se', 'Northern Sami'),
    ('sm', 'Samoan'),
    ('sg', 'Sango'),
    ('sr', 'Serbian'),
    ('gd', 'Scottish Gaelic, Gaelic'),
    ('sn', 'Shona'),
    ('si', 'Sinhala, Sinhalese'),
    ('sk', 'Slovak'),
    ('sl', 'Slovene'),
    ('so', 'Somali'),
    ('st', 'Southern Sotho'),
    ('es', 'Spanish'),
    ('su', 'Sundanese'),
    ('sw', 'Swahili'),
    ('ss', 'Swati'),
    ('sv', 'Swedish'),
    ('ta', 'Tamil'),
    ('te', 'Telugu'),
    ('tg', 'Tajik'),
    ('th', 'Thai'),
    ('ti', 'Tigrinya'),
    ('bo', 'Tibetan Standard, Tibetan, Central'),
    ('tk', 'Turkmen'),
    ('tl', 'Tagalog'),
    ('tn', 'Tswana'),
    ('to', 'Tonga (Tonga Islands)'),
    ('tr', 'Turkish'),
    ('ts', 'Tsonga'),
    ('tt', 'Tatar'),
    ('tw', 'Twi'),
    ('ty', 'Tahitian'),
    ('ug', 'Uyghur'),
    ('uk', 'Ukrainian'),
    ('ur', 'Urdu'),
    ('uz', 'Uzbek'),
    ('ve', 'Venda'),
    ('vi', 'Vietnamese'),
    ('vo', 'Volapük'),
    ('wa', 'Walloon'),
    ('cy', 'Welsh'),
    ('wo', 'Wolof'),
    ('fy', 'Western Frisian'),
    ('xh', 'Xhosa'),
    ('yi', 'Yiddish'),
    ('yo', 'Yoruba'),
    ('za', 'Zhuang, Chuang'),
    ('zu', 'Zulu'));

  Lang_native: array[0..184] of array[0..1] of string = (
    ('ab', 'аҧсуа бызшәа, аҧсшәа'),
    ('aa', 'Afaraf'),
    ('af', 'Afrikaans'),
    ('ak', 'Akan'),
    ('sq', 'Shqip'),
    ('am', 'አማርኛ'),
    ('ar', 'العربية'),
    ('an', 'aragonés'),
    ('hy', 'Հայերեն'),
    ('as', 'অসমীয়া'),
    ('av', 'авар мацӀ, магӀарул мацӀ'),
    ('ae', 'avesta'),
    ('ay', 'aymar aru'),
    ('az', 'azərbaycan dili'),
    ('bm', 'bamanankan'),
    ('ba', 'башҡорт теле'),
    ('eu', 'euskara, euskera'),
    ('be', 'беларуская мова'),
    ('bn', 'বাংলা'),
    ('bh', 'भोजपुरी'),
    ('bi', 'Bislama'),
    ('bs', 'bosanski jezik'),
    ('br', 'brezhoneg'),
    ('bg', 'български език'),
    ('my', 'ဗမာစာ'),
    ('ca', 'català'),
    ('ch', 'Chamoru'),
    ('ce', 'нохчийн мотт'),
    ('ny', 'chiCheŵa, chinyanja'),
    ('zh', '中文 (Zhōngwén), 汉语, 漢語'),
    ('cv', 'чӑваш чӗлхи'),
    ('kw', 'Kernewek'),
    ('co', 'corsu, lingua corsa'),
    ('cr', 'ᓀᐦᐃᔭᐍᐏᐣ'),
    ('hr', 'hrvatski jezik'),
    ('cs', 'čeština, český jazyk'),
    ('da', 'dansk'),
    ('dv', 'ދިވެހި'),
    ('nl', 'Nederlands, Vlaams'),
    ('dz', 'རྫོང་ཁ'),
    ('en', 'English'),
    ('eo', 'Esperanto'),
    ('et', 'eesti, eesti keel'),
    ('ee', 'Eʋegbe'),
    ('fo', 'føroyskt'),
    ('fj', 'vosa Vakaviti'),
    ('fi', 'suomi, suomen kieli'),
    ('fr', 'français, langue française'),
    ('ff', 'Fulfulde, Pulaar, Pular'),
    ('gl', 'galego'),
    ('ka', 'ქართული'),
    ('de', 'Deutsch'),
    ('el', 'ελληνικά'),
    ('gn', 'Avañe''ẽ'),
    ('gu', 'ગુજરાતી'),
    ('ht', 'Kreyòl ayisyen'),
    ('ha', '(Hausa) هَوُسَ'),
    ('he', 'עברית'),
    ('hz', 'Otjiherero'),
    ('hi', 'हिन्दी, हिंदी'),
    ('ho', 'Hiri Motu'),
    ('hu', 'magyar'),
    ('ia', 'Interlingua'),
    ('id', 'Bahasa Indonesia'),
    ('ie', 'Interlingue'),
    ('ga', 'Gaeilge'),
    ('ig', 'Asụsụ Igbo'),
    ('ik', 'Iñupiaq, Iñupiatun'),
    ('io', 'Ido'),
    ('is', 'Íslenska'),
    ('it', 'italiano'),
    ('iu', 'ᐃᓄᒃᑎᑐᑦ'),
    ('ja', '日本語 (にほんご)'),
    ('jv', 'basa Jawa'),
    ('kl', 'kalaallisut, kalaallit oqaasii'),
    ('kn', 'ಕನ್ನಡ'),
    ('kr', 'Kanuri'),
    ('ks', 'कश्मीरी, كشميري‎'),
    ('kk', 'қазақ тілі'),
    ('km', 'ខ្មែរ, ខេមរភាសា, ភាសាខ្មែរ'),
    ('ki', 'Gĩkũyũ'),
    ('rw', 'Ikinyarwanda'),
    ('ky', 'Кыргызча, Кыргыз тили'),
    ('kv', 'коми кыв'),
    ('kg', 'Kikongo'),
    ('ko', '한국어, 조선어'),
    ('ku', 'Kurdî, كوردی‎'),
    ('kj', 'Kuanyama'),
    ('la', 'latine, lingua latina'),
    ('lld', 'ladin, lingua ladina'),
    ('lb', 'Lëtzebuergesch'),
    ('lg', 'Luganda'),
    ('li', 'Limburgs'),
    ('ln', 'Lingála'),
    ('lo', 'ພາສາລາວ'),
    ('lt', 'lietuvių kalba'),
    ('lu', 'Tshiluba'),
    ('lv', 'latviešu valoda'),
    ('gv', 'Gaelg, Gailck'),
    ('mk', 'македонски јазик'),
    ('mg', 'fiteny malagasy'),
    ('ms', 'bahasa Melayu, بهاس ملايو‎'),
    ('ml', 'മലയാളം'),
    ('mt', 'Malti'),
    ('mi', 'te reo Māori'),
    ('mr', 'मराठी'),
    ('mh', 'Kajin M̧ajeļ'),
    ('mn', 'монгол'),
    ('na', 'Ekakairũ Naoero'),
    ('nv', 'Diné bizaad'),
    ('nd', 'isiNdebele'),
    ('ne', 'नेपाली'),
    ('ng', 'Owambo'),
    ('nb', 'Norsk bokmål'),
    ('nn', 'Norsk nynorsk'),
    ('no', 'Norsk'),
    ('ii', 'ꆈꌠ꒿ Nuosuhxop'),
    ('nr', 'isiNdebele'),
    ('oc', 'occitan, lenga d''òc'),
    ('oj', 'ᐊᓂᔑᓈᐯᒧᐎᓐ'),
    ('cu', 'ѩзыкъ словѣньскъ'),
    ('om', 'Afaan Oromoo'),
    ('or', 'ଓଡ଼ିଆ'),
    ('os', 'ирон æвзаг'),
    ('pa', 'ਪੰਜਾਬੀ, پنجابی‎'),
    ('pi', 'पाऴि'),
    ('fa', 'فارسی'),
    ('pl', 'język polski, polszczyzna'),
    ('ps', 'پښتو'),
    ('pt', 'português'),
    ('qu', 'Runa Simi, Kichwa'),
    ('rm', 'rumantsch grischun'),
    ('rn', 'Ikirundi'),
    ('ro', 'limba română'),
    ('ru', 'Русский'),
    ('sa', 'संस्कृतम्'),
    ('sc', 'sardu'),
    ('sd', 'सिन्धी, سنڌي، سندھی‎'),
    ('se', 'Davvisámegiella'),
    ('sm', 'gagana fa''a Samoa'),
    ('sg', 'yângâ tî sängö'),
    ('sr', 'српски језик'),
    ('gd', 'Gàidhlig'),
    ('sn', 'chiShona'),
    ('si', 'සිංහල'),
    ('sk', 'slovenčina, slovenský jazyk'),
    ('sl', 'slovenski jezik, slovenščina'),
    ('so', 'Soomaaliga, af Soomaali'),
    ('st', 'Sesotho'),
    ('es', 'español'),
    ('su', 'Basa Sunda'),
    ('sw', 'Kiswahili'),
    ('ss', 'SiSwati'),
    ('sv', 'svenska'),
    ('ta', 'தமிழ்'),
    ('te', 'తెలుగు'),
    ('tg', 'тоҷикӣ, toçikī, تاجیکی‎'),
    ('th', 'ไทย'),
    ('ti', 'ትግርኛ'),
    ('bo', 'བོད་ཡིག'),
    ('tk', 'Türkmen, Түркмен'),
    ('tl', 'Wikang Tagalog, ᜏᜒᜃᜅ᜔ ᜆᜄᜎᜓᜄ᜔'),
    ('tn', 'Setswana'),
    ('to', 'faka Tonga'),
    ('tr', 'Türkçe'),
    ('ts', 'Xitsonga'),
    ('tt', 'татар теле, tatar tele'),
    ('tw', 'Twi'),
    ('ty', 'Reo Tahiti'),
    ('ug', 'ئۇيغۇرچە‎, Uyghurche'),
    ('uk', 'українська мова'),
    ('ur', 'اردو'),
    ('uz', 'Oʻzbek, Ўзбек, أۇزبېك‎'),
    ('ve', 'Tshivenḓa'),
    ('vi', 'Việt Nam'),
    ('vo', 'Volapük'),
    ('wa', 'walon'),
    ('cy', 'Cymraeg'),
    ('wo', 'Wollof'),
    ('fy', 'Frysk'),
    ('xh', 'isiXhosa'),
    ('yi', 'ייִדיש'),
    ('yo', 'Yorùbá'),
    ('za', 'Saɯ cueŋƅ, Saw cuengh'),
    ('zu', 'isiZulu'));

  Country_name: array[0..248] of array[0..1] of string = (
    ('AD', 'Andorra'),
    ('AE', 'United Arab Emirates'),
    ('AF', 'Afghanistan'),
    ('AG', 'Antigua and Barbuda'),
    ('AI', 'Anguilla'),
    ('AL', 'Albania'),
    ('AM', 'Armenia'),
    ('AO', 'Angola'),
    ('AQ', 'Antarctica'),
    ('AR', 'Argentina'),
    ('AS', 'American Samoa'),
    ('AT', 'Austria'),
    ('AU', 'Australia'),
    ('AW', 'Aruba'),
    ('AX', 'Åland Islands'),
    ('AZ', 'Azerbaijan'),
    ('BA', 'Bosnia and Herzegovina'),
    ('BB', 'Barbados'),
    ('BD', 'Bangladesh'),
    ('BE', 'Belgium'),
    ('BF', 'Burkina Faso'),
    ('BG', 'Bulgaria'),
    ('BH', 'Bahrain'),
    ('BI', 'Burundi'),
    ('BJ', 'Benin'),
    ('BL', 'Saint Barthélemy'),
    ('BM', 'Bermuda'),
    ('BN', 'Brunei Darussalam'),
    ('BO', 'Bolivia, Plurinational State of'),
    ('BQ', 'Bonaire, Sint Eustatius and Saba'),
    ('BR', 'Brazil'),
    ('BS', 'Bahamas'),
    ('BT', 'Bhutan'),
    ('BV', 'Bouvet Island'),
    ('BW', 'Botswana'),
    ('BY', 'Belarus'),
    ('BZ', 'Belize'),
    ('CA', 'Canada'),
    ('CC', 'Cocos (Keeling) Islands'),
    ('CD', 'Congo, the Democratic Republic of the'),
    ('CF', 'Central African Republic'),
    ('CG', 'Congo'),
    ('CH', 'Switzerland'),
    ('CI', 'Côte d''Ivoire'),
    ('CK', 'Cook Islands'),
    ('CL', 'Chile'),
    ('CM', 'Cameroon'),
    ('CN', 'China'),
    ('CO', 'Colombia'),
    ('CR', 'Costa Rica'),
    ('CU', 'Cuba'),
    ('CV', 'Cabo Verde'),
    ('CW', 'Curaçao'),
    ('CX', 'Christmas Island'),
    ('CY', 'Cyprus'),
    ('CZ', 'Czech Republic'),
    ('DE', 'Germany'),
    ('DJ', 'Djibouti'),
    ('DK', 'Denmark'),
    ('DM', 'Dominica'),
    ('DO', 'Dominican Republic'),
    ('DZ', 'Algeria'),
    ('EC', 'Ecuador'),
    ('EE', 'Estonia'),
    ('EG', 'Egypt'),
    ('EH', 'Western Sahara'),
    ('ER', 'Eritrea'),
    ('ES', 'Spain'),
    ('ET', 'Ethiopia'),
    ('FI', 'Finland'),
    ('FJ', 'Fiji'),
    ('FK', 'Falkland Islands (Malvinas)'),
    ('FM', 'Micronesia, Federated States of'),
    ('FO', 'Faroe Islands'),
    ('FR', 'France'),
    ('GA', 'Gabon'),
    ('GB', 'United Kingdom of Great Britain and Northern Ireland'),
    ('GD', 'Grenada'),
    ('GE', 'Georgia'),
    ('GF', 'French Guiana'),
    ('GG', 'Guernsey'),
    ('GH', 'Ghana'),
    ('GI', 'Gibraltar'),
    ('GL', 'Greenland'),
    ('GM', 'Gambia'),
    ('GN', 'Guinea'),
    ('GP', 'Guadeloupe'),
    ('GQ', 'Equatorial Guinea'),
    ('GR', 'Greece'),
    ('GS', 'South Georgia and the South Sandwich Islands'),
    ('GT', 'Guatemala'),
    ('GU', 'Guam'),
    ('GW', 'Guinea-Bissau'),
    ('GY', 'Guyana'),
    ('HK', 'Hong Kong'),
    ('HM', 'Heard Island and McDonald Islands'),
    ('HN', 'Honduras'),
    ('HR', 'Croatia'),
    ('HT', 'Haiti'),
    ('HU', 'Hungary'),
    ('ID', 'Indonesia'),
    ('IE', 'Ireland'),
    ('IL', 'Israel'),
    ('IM', 'Isle of Man'),
    ('IN', 'India'),
    ('IO', 'British Indian Ocean Territory'),
    ('IQ', 'Iraq'),
    ('IR', 'Iran, Islamic Republic of'),
    ('IS', 'Iceland'),
    ('IT', 'Italy'),
    ('JE', 'Jersey'),
    ('JM', 'Jamaica'),
    ('JO', 'Jordan'),
    ('JP', 'Japan'),
    ('KE', 'Kenya'),
    ('KG', 'Kyrgyzstan'),
    ('KH', 'Cambodia'),
    ('KI', 'Kiribati'),
    ('KM', 'Comoros'),
    ('KN', 'Saint Kitts and Nevis'),
    ('KP', 'Korea, Democratic People''s Republic of'),
    ('KR', 'Korea, Republic of'),
    ('KW', 'Kuwait'),
    ('KY', 'Cayman Islands'),
    ('KZ', 'Kazakhstan'),
    ('LA', 'Lao People''s Democratic Republic'),
    ('LB', 'Lebanon'),
    ('LC', 'Saint Lucia'),
    ('LI', 'Liechtenstein'),
    ('LK', 'Sri Lanka'),
    ('LR', 'Liberia'),
    ('LS', 'Lesotho'),
    ('LT', 'Lithuania'),
    ('LU', 'Luxembourg'),
    ('LV', 'Latvia'),
    ('LY', 'Libya'),
    ('MA', 'Morocco'),
    ('MC', 'Monaco'),
    ('MD', 'Moldova, Republic of'),
    ('ME', 'Montenegro'),
    ('MF', 'Saint Martin (French part)'),
    ('MG', 'Madagascar'),
    ('MH', 'Marshall Islands'),
    ('MK', 'Macedonia, the former Yugoslav Republic of'),
    ('ML', 'Mali'),
    ('MM', 'Myanmar'),
    ('MN', 'Mongolia'),
    ('MO', 'Macao'),
    ('MP', 'Northern Mariana Islands'),
    ('MQ', 'Martinique'),
    ('MR', 'Mauritania'),
    ('MS', 'Montserrat'),
    ('MT', 'Malta'),
    ('MU', 'Mauritius'),
    ('MV', 'Maldives'),
    ('MW', 'Malawi'),
    ('MX', 'Mexico'),
    ('MY', 'Malaysia'),
    ('MZ', 'Mozambique'),
    ('NA', 'Namibia'),
    ('NC', 'New Caledonia'),
    ('NE', 'Niger'),
    ('NF', 'Norfolk Island'),
    ('NG', 'Nigeria'),
    ('NI', 'Nicaragua'),
    ('NL', 'Netherlands'),
    ('NO', 'Norway'),
    ('NP', 'Nepal'),
    ('NR', 'Nauru'),
    ('NU', 'Niue'),
    ('NZ', 'New Zealand'),
    ('OM', 'Oman'),
    ('PA', 'Panama'),
    ('PE', 'Peru'),
    ('PF', 'French Polynesia'),
    ('PG', 'Papua New Guinea'),
    ('PH', 'Philippines'),
    ('PK', 'Pakistan'),
    ('PL', 'Poland'),
    ('PM', 'Saint Pierre and Miquelon'),
    ('PN', 'Pitcairn'),
    ('PR', 'Puerto Rico'),
    ('PS', 'Palestine, State of'),
    ('PT', 'Portugal'),
    ('PW', 'Palau'),
    ('PY', 'Paraguay'),
    ('QA', 'Qatar'),
    ('RE', 'Réunion'),
    ('RO', 'Romania'),
    ('RS', 'Serbia'),
    ('RU', 'Russian Federation'),
    ('RW', 'Rwanda'),
    ('SA', 'Saudi Arabia'),
    ('SB', 'Solomon Islands'),
    ('SC', 'Seychelles'),
    ('SD', 'Sudan'),
    ('SE', 'Sweden'),
    ('SG', 'Singapore'),
    ('SH', 'Saint Helena, Ascension and Tristan da Cunha'),
    ('SI', 'Slovenia'),
    ('SJ', 'Svalbard and Jan Mayen'),
    ('SK', 'Slovakia'),
    ('SL', 'Sierra Leone'),
    ('SM', 'San Marino'),
    ('SN', 'Senegal'),
    ('SO', 'Somalia'),
    ('SR', 'Suriname'),
    ('SS', 'South Sudan'),
    ('ST', 'Sao Tome and Principe'),
    ('SV', 'El Salvador'),
    ('SX', 'Sint Maarten (Dutch part)'),
    ('SY', 'Syrian Arab Republic'),
    ('SZ', 'Swaziland'),
    ('TC', 'Turks and Caicos Islands'),
    ('TD', 'Chad'),
    ('TF', 'French Southern Territories'),
    ('TG', 'Togo'),
    ('TH', 'Thailand'),
    ('TJ', 'Tajikistan'),
    ('TK', 'Tokelau'),
    ('TL', 'Timor-Leste'),
    ('TM', 'Turkmenistan'),
    ('TN', 'Tunisia'),
    ('TO', 'Tonga'),
    ('TR', 'Turkey'),
    ('TT', 'Trinidad and Tobago'),
    ('TV', 'Tuvalu'),
    ('TW', 'Taiwan, Province of China'),
    ('TZ', 'Tanzania, United Republic of'),
    ('UA', 'Ukraine'),
    ('UG', 'Uganda'),
    ('UM', 'United States Minor Outlying Islands'),
    ('US', 'United States of America'),
    ('UY', 'Uruguay'),
    ('UZ', 'Uzbekistan'),
    ('VA', 'Holy See'),
    ('VC', 'Saint Vincent and the Grenadines'),
    ('VE', 'Venezuela, Bolivarian Republic of'),
    ('VG', 'Virgin Islands, British'),
    ('VI', 'Virgin Islands, U.S.'),
    ('VN', 'Viet Nam'),
    ('VU', 'Vanuatu'),
    ('WF', 'Wallis and Futuna'),
    ('WS', 'Samoa'),
    ('YE', 'Yemen'),
    ('YT', 'Mayotte'),
    ('ZA', 'South Africa'),
    ('ZM', 'Zambia'),
    ('ZW', 'Zimbabwe'));

  ldir: array[0..3] of string =
    ('LANG', 'languages', 'locale', 'locale' + PathDelim + 'LC_MESSAGES');

var
  AvailableLanguages: TStringList;
  LastSelected: string = '';
  LangDir: string = '';

  procedure CollectLanguagesFiles(appname: string = ''; dir: string = ''; useNativeName: Boolean = True);
  function GetLangName(lcode: string; useNativeName: Boolean = True): string;

  function SetLang(Lang: string): Boolean;
  function SetLangByIndex(Idx: Integer): Boolean;
  function GetDefaultLang: string;

implementation

function SortValue(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2]);
end;

procedure CollectLanguagesFiles(appname: string; dir: string;
  useNativeName: Boolean);

  procedure searchLangDir(adir, aname: string);
  var
    SR: TSearchRec;
    p: Integer;
    s, id: string;
  begin
    if RightStr(adir, 1) <> PathDelim then
      adir := adir + PathDelim;
    if DirectoryExistsUTF8(adir) then
    begin
      aname := LowerCase(aname);
      if FindFirstUTF8(adir + '*', faAnyFile, SR) = 0 then
        repeat
          s := LowerCase(SR.Name);
          if (Pos(aname + '.', s) = 1) and
            ((RightStr(s, 3) = '.po') or (RightStr(s, 3) = '.mo')) then
          begin
            s := SR.Name;
            SetLength(s, Length(s) - 3);
            p := Pos('.', s);
            if p > 0 then
            begin
              id := Copy(s, p + 1, Length(s));
              if AvailableLanguages.Values[id] = '' then
                AvailableLanguages.Values[id] := GetLangName(id, useNativeName);
            end;
          end;
          until FindNextUTF8(SR) <> 0;
        FindCloseUTF8(SR);
        if AvailableLanguages.Count > 0 then
          AvailableLanguages.CustomSort(@SortValue);
    end;
  end;

var
  sdir: string;
  lauto: Boolean = False;
  i: Integer;
begin
  if dir = '' then
  begin
    if LangDir <> '' then
      dir := LangDir
    else
    begin
      dir := GetCurrentDirUTF8 + PathDelim;
      lauto := True;
    end;
  end;
  if appname = '' then
    appname := ExtractFileNameOnly(ParamStrUTF8(0));
  AvailableLanguages.Clear;

  if lauto then
    for i := Low(ldir) to High(ldir) do
    begin
      sdir := dir + ldir[i] + PathDelim;
      searchLangDir(sdir, appname);
    end
  else
    searchLangDir(dir, appname);
end;

function GetLangName(lcode: string; useNativeName: Boolean): string;

  function GetLang(const l: string): string;
  var
    i: Integer;
  begin
    Result := l;
    for i := Low(Lang_english) to High(Lang_english) do
    begin
      if SameText(l, Lang_english[i, 0]) then
      begin
        Result := Lang_english[i, 1];
        Break;
      end;
    end;
  end;

  function GetLangNative(const l: string): string;
  var
    i: Integer;
  begin
    Result := l;
    for i := Low(Lang_native) to High(Lang_native) do
    begin
      if SameText(l, Lang_native[i, 0]) then
      begin
        Result := Lang_native[i, 1];
        Break;
      end;
    end;
  end;

  function GetCountry(l: string): string;
  var
    i: Integer;
  begin
    Result := l;
    for i := Low(Country_name) to High(Country_name) do
    begin
      if SameText(l, Country_name[i, 0]) then
      begin
        Result := Country_name[i, 1];
        Break;
      end;
    end;
  end;

var
  p: Integer;
  s, id: String;
begin
  Result := lcode;
  if Result = '' then Exit;
  s := TrimSet(lcode,[' ','.','_','-']);
  p := Pos('_', s);
  if p = 0 then
    p := Pos('-', s);
  if p > 1 then
  begin
    id := Copy(s, 1, p - 1);
    if useNativeName then
      Result := GetLangNative(id)
    else
      Result := GetLang(id);
    id := Copy(s, P + 1, Length(s));
    s := GetCountry(id);
    if s <> id then
      Result := Format('%s (%s)', [Result, GetCountry(id)]);
  end
  else
    Result := GetLang(s);
end;

function TranslateLCL(Lang: string): Boolean;
var
  lcllangdir, lcllangpath, fallbacklang: string;
  lcllangfound: Boolean = False;
  i: Integer;

  procedure SearchLangPath;
  begin
    if FileExistsUTF8(lcllangdir + 'lclstrconsts.' + Lang + '.po') then
    begin
      lcllangfound := True;
      lcllangpath := lcllangdir + 'lclstrconsts.' + Lang + '.po';
    end;
  end;

begin
  Result := False;
  if LangDir <> '' then
  begin
    lcllangdir := LangDir;
    if RightStr(lcllangdir, 1) <> PathDelim then
      lcllangdir := lcllangdir + PathDelim;
    SearchLangPath;
  end
  else
  begin
    for i := Low(ldir) to High(ldir) do
    begin
      lcllangdir := GetCurrentDirUTF8 + PathDelim + ldir[i];
      SearchLangPath;
      if lcllangfound then
        Break;
    end;
  end;
  if lcllangfound then
  begin
    fallbacklang := '';
    TranslateUnitResourceStrings('LclStrConsts', lcllangpath, Lang, fallbacklang);
    Result := True;
  end;
end;

function SetLang(Lang: string): Boolean;
begin
  Result := False;
  if (LastSelected <> Lang) and
    (AvailableLanguages.IndexOfName(lang) > 0) then
  begin
    SetDefaultLang(Lang);
    LastSelected := Lang;
    TranslateLCL(Lang);
    Result := True;
  end;
end;

function SetLangByIndex(Idx: Integer): Boolean;
begin
  Result := False;
  if Idx < 0 then Exit;
  if LastSelected <> AvailableLanguages.Names[idx] then
  begin
    SetDefaultLang(AvailableLanguages.Names[Idx]);
    LastSelected := AvailableLanguages.Names[Idx];
    TranslateLCL(AvailableLanguages.Names[Idx]);
    Result := True;
  end;
end;

function GetDefaultLang: string;
begin
  {$IF FPC_FULLVERSION >= 20701}
  Result := LCLTranslator.GetDefaultLang;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

initialization
  AvailableLanguages := TStringList.Create;
  AvailableLanguages.NameValueSeparator := '=';

finalization
  AvailableLanguages.Free;

end.

