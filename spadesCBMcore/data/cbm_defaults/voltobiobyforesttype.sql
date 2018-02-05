SELECT a.* FROM vol_to_bio_factor a
INNER JOIN vol_to_bio_forest_type b ON a.id=b.vol_to_bio_factor_id
INNER JOIN species ON b.forest_type_id == species.forest_type_id
WHERE b.spatial_unit_id=%1.0f AND species.id=%1.0f
