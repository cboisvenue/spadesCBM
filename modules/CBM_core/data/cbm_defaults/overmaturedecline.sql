select 
spatial_unit.id as spuid,
turnover_parameter.coarse_ag_split, 
turnover_parameter.fine_ag_split, 
turnover_parameter.branch_snag_split
 from turnover_parameter
inner join eco_boundary on turnover_parameter.id = eco_boundary.turnover_parameter_id
inner join spatial_unit on eco_boundary.id = spatial_unit.eco_boundary_id
order by spatial_unit.id
