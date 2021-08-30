# Admin_Disturbance

### Datasets

- [Harris C-flux data](https://www.nature.com/articles/s41558-020-00976-6)
- Ontario disturbance data
  - [Fire](https://geohub.lio.gov.on.ca/datasets/fire-disturbance-area/explore)
  - [Insect](https://geohub.lio.gov.on.ca/datasets/forest-insect-damage-event/explore?location=50.926000%2C-84.745000%2C5.00)
  - [Abiotic](https://geohub.lio.gov.on.ca/datasets/forest-abiotic-damage-event/explore?location=50.926000%2C-84.745000%2C5.00)
  - [Disease](https://geohub.lio.gov.on.ca/datasets/forest-disease-damage-event/explore)
- [Ontario FRI data](https://geohub.lio.gov.on.ca/documents/2ecc0cc57d1d4d58b5d3540fd0ee2cbd/about)
- [Ontario waterbodies](https://geohub.lio.gov.on.ca/datasets/22bab3c9f37a4dd0845eb89e7b247a9f/explore)
- [Ontario land management zone](https://www.gisapplication.lrc.gov.on.ca/CLUPA/Index.html?site=CLUPA&viewer=CLUPA&locale=en-US)

### Project workflow

#### Preprocessing

- [x] Validate polygons
  - [x] Admin zones

- [x] Clean FRI data
  - [x] Select common columns
  - [x] merge to single GPKG
- [ ] Rasterize data
  - [ ] FRI
    - [x] Forest origin year
      - [x] Crop to AoU
      - [x] Remove lakes
    - [ ] Protection status
      - [ ] Crop to AoU
      - [ ] Remove lakes
  - [x] Disturbance
    - [x] Sum dist across year 
    - [x] Crop to AoU
    - [x] Remove lakes
    - [x] Mask by FRI raster
- [x] Clean C-flux data
  - [x] Crop to Aou
  - [x] Remove lakes
  - [x] Mask by FRI raster
- [ ] Extract raster by admin zone
  - [x] C-flux
  - [ ] Dist
    - [ ] get mean prop dist for only forest areas
      - [ ] Rasterize by year
      - [ ] mask by fri
      - [ ] extract in admin
  - [x] Forest Age
  - [ ] Protection
- [ ] Join to Df

#### Modelling

- [ ] Spatial model
  - [ ] C-flux ~ admin*prop_dist
- [ ] Linear model
  - [ ] Forest age ~ admin

#### Outputs

- [ ] Figures
  - [ ] C-flux ~ admin*prop_dist line plot
  - [ ] Forest age by zone dot-whisker plot
- [ ] Stats
  - [ ] Proportion forest area protected in each admin zone

