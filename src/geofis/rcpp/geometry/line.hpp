/**
 * GeoFIS R package
 *
 * Copyright (C) 2021 INRAE
 *
 * Authors:
 * 	Jean-luc Lablée - INRAE
 * 	Serge Guillaume - INRAE
 *
 * License: CeCILL v2.1
 * 	https://cecill.info/licences/Licence_CeCILL_V2.1-en.html
 * 	https://cecill.info/licences/Licence_CeCILL_V2.1-fr.html
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "https://cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights, and the successive licensors have only limited liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 */
#ifndef H5EA144C7_9249_46FC_9E76_8829D0086A77
#define H5EA144C7_9249_46FC_9E76_8829D0086A77

#include <Rcpp.h>
#include <CGAL/Point_2.h>
#include <CGAL/double.h>

namespace geofis {

template <class Kernel> Rcpp::S4 make_rcpp_line(const CGAL::Point_2<Kernel> &point1, const CGAL::Point_2<Kernel> &point2) {
	Rcpp::NumericVector coord_values = { CGAL::to_double(point1.x()), CGAL::to_double(point2.x()), CGAL::to_double(point1.y()), CGAL::to_double(point2.y()) };
	Rcpp::NumericMatrix coords(2, 2, coord_values.begin());
	colnames(coords) = Rcpp::CharacterVector::create("x", "y");
	Rcpp::Function line_function("Line");
	return line_function(Rcpp::Named("coords") = coords);
}

} // namespace geofis

#endif // H5EA144C7_9249_46FC_9E76_8829D0086A77 