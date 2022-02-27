## code to prepare `nn_base_color_palette` dataset goes here
nn_base_color_palette = tibble::tribble(
  ~color_name, ~hex_code, ~color_category,
  'NN Blue','#006d9d', 'Primary',
  'NN Teal',	'#00a4b9',	'Accent',
  'NN Gold',	'#fbab18',	'Accent',
  'NN Red',	'#d63f3e',	'Accent',
  'NN Mauve',	'#a1486f',	'Accent',
  'NN Midnight',	'#002934',	'Background',
  'NN Harbor',	'#ebebeb',	'Background',
  'NN Black',	'#000000',	'Background',
  'NN White',	'#FFFFFF',	'Background',
  'NN Sky',	'#33a8df',	'Supplemental',
  'NN Green',	'#4ea652',	'Supplemental',
  'NN Pear',	'#ccc72c',	'Supplemental',
  'NN Lemon',	'#ffcb05',	'Supplemental',
  'NN Carrot',	'#f47d20',	'Supplemental',
  'NN Magenta',	'#cc3d72',	'Supplemental',
  'NN Pink',	'#f597b2',	'Supplemental',
  'NN Purple',	'#915ea6',	'Supplemental',
  'NN Chestnut',	'#644a49',	'Supplemental'
)

usethis::use_data(nn_base_color_palette, overwrite = TRUE)
