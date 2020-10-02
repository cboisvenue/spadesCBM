select spatial_unit.id as spuid, 
 stump_parameter.sw_top_proportion as softwood_top_proportion, 
 stump_parameter.sw_stump_proportion as softwood_stump_proportion,
 stump_parameter.hw_top_proportion as hardwood_top_proportion,
 stump_parameter.hw_stump_proportion as hardwood_stump_proportion 
from stump_parameter
inner join admin_boundary on admin_boundary.stump_parameter_id = stump_parameter.id
inner join spatial_unit on admin_boundary.id = spatial_unit.admin_boundary_id
