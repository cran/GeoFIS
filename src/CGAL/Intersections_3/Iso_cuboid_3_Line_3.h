// Copyright (c) 2010 GeometryFactory (France).
// All rights reserved.
//
// This file is part of CGAL (www.cgal.org)
//
// $URL: https://github.com/CGAL/cgal/blob/v5.1.1/Intersections_3/include/CGAL/Intersections_3/Iso_cuboid_3_Line_3.h $
// $Id: Iso_cuboid_3_Line_3.h 52164b1 2019-10-19T15:34:59+02:00 Sébastien Loriot
// SPDX-License-Identifier: LGPL-3.0-or-later OR LicenseRef-Commercial
//
//
// Author(s)     : Sebastien Loriot
//

#ifndef CGAL_INTERSECTIONS_3_ISO_CUBOID_3_LINE_3_H
#define CGAL_INTERSECTIONS_3_ISO_CUBOID_3_LINE_3_H

#include <CGAL/Intersections_3/internal/intersection_3_1_impl.h>
#include <CGAL/Iso_cuboid_3.h>
#include <CGAL/Line_3.h>

namespace CGAL {
  CGAL_DO_INTERSECT_FUNCTION(Iso_cuboid_3,Line_3, 3)
  CGAL_INTERSECTION_FUNCTION(Line_3, Iso_cuboid_3, 3)
}

#endif // CGAL_INTERSECTIONS_3_BBOX_3_LINE_3_H
