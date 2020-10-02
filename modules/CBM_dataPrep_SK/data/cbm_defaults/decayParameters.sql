SELECT 
    dom_pool_id as SoilPoolID,
    base_decay_rate as OrganicMatterDecayRate,
    reference_temp as ReferenceTemp,
    q10 as Q10,
    prop_to_atmosphere as PropToAtmosphere,
    max_rate as MaxDecayRate
FROM decay_parameter
