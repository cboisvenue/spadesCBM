select spatial_unit.id as SpatialUnitID, climate.mean_annual_temperature as MeanAnnualTemperature from climate 
inner join spatial_unit on climate.climate_time_series_id = spatial_unit.climate_time_series_id
where climate.t_year = -1
order by spatial_unit.id
