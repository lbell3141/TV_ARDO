library(raster)

test_scene_path <- "./data/testAOI_psscene_analytic_sr_udm2/PSScene/20230817_175040_98_2480_3B_AnalyticMS_SR_clip.tif"
test_data <- raster(test_scene_path)
plot(test_data)


test_scene_path <- "./data/testAOI_psscene_analytic_sr_udm2/PSScene/20230817_175040_98_2480_3B_udm2_clip.tif"
test_data <- raster(test_scene_path)
plot(test_data)

test