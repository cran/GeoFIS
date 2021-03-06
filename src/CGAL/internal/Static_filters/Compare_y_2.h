// Copyright (c) 2011 GeometryFactory Sarl (France)
// All rights reserved.
//
// This file is part of CGAL (www.cgal.org)
//
// $URL: https://github.com/CGAL/cgal/blob/v5.1.1/Filtered_kernel/include/CGAL/internal/Static_filters/Compare_y_2.h $
// $Id: Compare_y_2.h 0779373 2020-03-26T13:31:46+01:00 Sébastien Loriot
// SPDX-License-Identifier: LGPL-3.0-or-later OR LicenseRef-Commercial
//
//
// Author(s)     : Andreas Fabri


#ifndef CGAL_INTERNAL_STATIC_FILTERS_COMPARE_Y_2_H
#define CGAL_INTERNAL_STATIC_FILTERS_COMPARE_Y_2_H

#include <CGAL/Bbox_2.h>
#include <CGAL/Profile_counter.h>
#include <CGAL/internal/Static_filters/tools.h>
#include <cmath>
#include <iostream>

namespace CGAL {

namespace internal {

namespace Static_filters_predicates {


template < typename K_base >
class Compare_y_2
  : public K_base::Compare_y_2
{
  typedef typename K_base::Point_2   Point_2;
  typedef typename K_base::Line_2    Line_2;
  typedef typename K_base::Compare_y_2   Base;

public:

  typedef typename Base::result_type  result_type;


#ifndef CGAL_CFG_MATCHING_BUG_6
  using Base::operator();
#else // CGAL_CFG_MATCHING_BUG_6
  template <typename T>
  result_type
  operator()(const T& t1, const T& t2, const T& t3) const
  {
    return Base()(t1,t2,t3);
  }

  template <typename T>
  result_type
  operator()(const T& t1, const T& t2, const T& t3, const T& t4) const
  {
    return Base()(t1,t2,t3, t4);
  }

  result_type
  operator()(const Point_2& p, const Line_2& l1, const Line_2& l2) const
  {
    return Base()(p,l1,l2);
  }
#endif // CGAL_CFG_MATCHING_BUG_6


  result_type operator()(const Point_2 &p, const Point_2& q) const
  {
    CGAL_BRANCH_PROFILER(std::string("semi-static attempts/calls to   : ") +
                         std::string(CGAL_PRETTY_FUNCTION), tmp);

    Get_approx<Point_2> get_approx; // Identity functor for all points
                                    // but lazy points
    double py, qy;

    if (fit_in_double(get_approx(p).y(), py) && fit_in_double(get_approx(q).y(), qy) )
    {
      CGAL_BRANCH_PROFILER_BRANCH(tmp);
      return compare(py, qy);
    }
    return Base::operator()(p, q);
  }



}; // end class Compare_y_2

} // end namespace Static_filters_predicates

} // end namespace internal

} // end namespace CGAL

#endif  // CGAL_INTERNAL_STATIC_FILTERS_COMPARE_Y_2_H
