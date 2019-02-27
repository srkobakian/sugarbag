# add global variables

# allocate function
utils::globalVariables(c('focal_distance',
    'points',
    'angle',
    'hyp',
    'SA2_5DIG16',
    ':=',
    "sf_id",
    "longitude",
    "latitude", "assigned", "f_width"))

# create_buffer function
utils::globalVariables(c('lat_int',
'long_int',
'long_int',
'long_min',
'long_max',
'lat_int',
'lat_min',
'lat_max',
'hex_long_int',
'mean_long_min',
'mean_long_max',
'hex_lat_int',
'mean_lat_min',
'mean_lat_max',
'lat_buffer',
'long_buffer'))

# create centroids function
utils::globalVariables(c('X', 'Y', 'projstring'))

# filter_grid_points function
utils::globalVariables(c('hex_filter', 'width', 'hex_size', 'hex_lat_c', 'hex_long_c', 'latitude1', 'longitude1', 'angle', 'hyp', 'hex_angle',
'angle_minus', 'angle_plus'))

# find lat group function
utils::globalVariables(c('next_lat', 'gradient', 'new_grad', 'cut_long'))


# create_hexmap function
utils::globalVariables(c('closest',
    'data', 'nest', 'unnest'))

# fortify hexagon function
utils::globalVariables(c('hexv_lat', 'hexv_long'))
