dict_fixest <- c(dist_km = "Distance",
                 "as.factor(order)" = "Order",
                 "dist_order" = "Downstream x Order",
                 "as.factor(dist_order)1" = "Downstream x Order $=$ 1",
                 downstream = "Downstream",
                 elevation = "Elevation",
                 slope = "Slope",
                 tmp_max = "Yearly Max. Temperature",
                 precipitation = "Yearly Precipitation",
                 accessibility_to_cities_2015 = "Accessibility in 2015",
                 pop_2015 = "Population in 2015",
                 "I(dist_km^2)" = "Distance$^2$",

                 # make dictionary anew, with all sorts of dependents
                 max_EVI_16_nomask = "Max EVI (16d, no mask)",
                 mean_EVI_16_nomask = "Mean EVI (16d, no mask)",
                 max_EVI_px_nomask = "Max EVI (pixel, no mask)",

                 max_EVI_16_cci_veg_broad = "Max EVI (16d, broad veg)",
                 mean_EVI_16_cci_veg_broad = "Mean EVI (16d, broad veg)",
                 max_EVI_px_cci_veg_broad = "Max EVI (pixel, broad veg)",

                 max_EVI_16_cci_veg_narrow = "Max EVI (16d, narrow veg)",
                 mean_EVI_16_cci_veg_narrow = "Mean EVI (16d, narrow veg)",
                 max_EVI_px_cci_veg_narrow = "Max EVI (pixel, narrow veg)",

                 max_EVI_16_cci_veg_fs = "Max EVI (16d, flooded/sparse veg)",
                 mean_EVI_16_cci_veg_fs = "Mean EVI (16d, flooded/sparse veg)",
                 max_EVI_px_cci_veg_fs = "Max EVI (pixel, flooded/sparse veg)",

                 max_EVI_16_cci_c_broad = "Max C EVI (16d, CCI-broad)",
                 mean_EVI_16_cci_c_broad = "Mean C EVI (16d, CCI-broad)",
                 max_EVI_px_cci_c_broad = "Max C EVI (pixel, CCI-broad)",

                 max_EVI_16_cci_c_narrow = "Max C EVI (16d, CCI-narrow)",
                 mean_EVI_16_cci_c_narrow = "Mean C EVI (16d, CCI-narrow)",
                 max_EVI_px_cci_c_narrow = "Max C EVI (pixel, CCI-narrow)",

                 max_EVI_16_cci_c_rainfed = "Max C EVI (16d, CCI-rainfed)",
                 mean_EVI_16_cci_c_rainfed = "Mean C EVI (16d, CCI-rainfed)",
                 max_EVI_px_cci_c_rainfed = "Max C EVI (pixel, CCI-rainfed)",

                 max_EVI_16_cci_c_irrigated = "Max C EVI (16d, CCI-irrigated)",
                 mean_EVI_16_cci_c_irrigated = "Mean C EVI (16d, CCI-irrigated)",
                 max_EVI_px_cci_c_irrigated = "Max C EVI (pixel, CCI-irrigated)",

                 max_EVI_16_af_c = "Max C EVI (16d, AF)",
                 mean_EVI_16_af_c = "Mean C EVI (16d, AF)",
                 max_EVI_px_af_c = "Max C EVI (pixel, AF)",

                 max_EVI_16_esri_c = "Max C EVI (16d, ESRI)",
                 mean_EVI_16_esri_c = "Mean C EVI (16d, ESRI)",
                 max_EVI_px_esri_c = "Max C EVI (pixel, ESRI)")