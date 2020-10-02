select 
    spatial_unit.id as SpatialUnitID,
    admin_boundary.id as AdminBoundaryID,
    eco_boundary.id as EcoBoundaryID
from spatial_unit
inner join admin_boundary on admin_boundary.id = spatial_unit.admin_boundary_id
inner join eco_boundary on eco_boundary.id = spatial_unit.eco_boundary_id
