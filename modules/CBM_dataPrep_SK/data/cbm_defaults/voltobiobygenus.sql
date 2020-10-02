SELECT b.spatial_unit_id, species.id, a.* 
FROM vol_to_bio_factor a
INNER JOIN vol_to_bio_genus b ON a.id=b.vol_to_bio_factor_id
INNER JOIN species ON b.genus_id == species.genus_id
WHERE 
    b.spatial_unit_id=26 AND species.id in(65)
    
    
