SELECT b.spatial_unit_id, b.species_id, a.* FROM vol_to_bio_factor a
INNER JOIN vol_to_bio_species b ON a.id=b.vol_to_bio_factor_id
