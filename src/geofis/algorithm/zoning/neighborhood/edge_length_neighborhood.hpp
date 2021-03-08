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
#ifndef EDGE_LENGTH_NEIGHBORHOOD_HPP_
#define EDGE_LENGTH_NEIGHBORHOOD_HPP_

#include <util/double.h>
#include <geofis/algorithm/zoning/zone_join.hpp>

namespace geofis {

struct edge_length_neighborhood {

	typedef bool result_type;

	double edge_length;

	edge_length_neighborhood() : edge_length(0) {}
	edge_length_neighborhood(double edge_length) : edge_length(edge_length) {}

	template <class Zone> result_type operator()(const Zone &zone1, const Zone &zone2) const {
		return UTIL_DOUBLE_GREATER_EQUAL(zone_join_length(zone1, zone2), edge_length);
	}

	bool operator==(const edge_length_neighborhood &other) const {
		return UTIL_DOUBLE_EQUAL(edge_length, other.edge_length);
	}
};

} // namespace geofis

#endif /* EDGE_LENGTH_NEIGHBORHOOD_HPP_ */