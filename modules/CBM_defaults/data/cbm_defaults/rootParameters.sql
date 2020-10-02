select 
    spatial_unit.id as SpatialUnitID,
    root_parameter.hw_a as rb_hw_a,
    root_parameter.sw_a as rb_sw_a,
    root_parameter.hw_b as rb_hw_b,
    root_parameter.frp_a,
    root_parameter.frp_b,
    root_parameter.frp_c
from root_parameter
    inner join spatial_unit on root_parameter.id = spatial_unit.root_parameter_id
