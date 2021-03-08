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
#ifndef H93626802_53A7_498E_99B0_56EAA9CCC9C6
#define H93626802_53A7_498E_99B0_56EAA9CCC9C6

#include <geofis/process/zoning/zoning_process_traits.hpp>

namespace geofis {

struct fusion_process_traits {

	typedef zoning_process_traits::aggregation_type aggregation_type;
	typedef zoning_process_traits::zone_distance_type zone_distance_type;
	typedef zoning_process_traits::multidimensional_distance_type multidimensional_distance_type;
	typedef zoning_process_traits::feature_distance_type feature_distance_type;
	typedef zoning_process_traits::attribute_distance_range_type attribute_distance_range_type;

	typedef zoning_process_traits::feature_range_type feature_range_type;
	typedef zoning_process_traits::zone_neighbor_range_type zone_neighbor_range_type;

	typedef zoning_process_traits::attribute_distance_type attribute_distance_type;

	typedef zoning_process_traits::feature_type feature_type;

	typedef zoning_process_traits::zone_type zone_type;

	typedef zoning_process_traits::zone_fusion_type zone_fusion_type;
	typedef zoning_process_traits::zone_fusion_container_type zone_fusion_container_type;

	typedef zoning_process_traits::zone_info_policy_type zone_info_policy_type;
	typedef zoning_process_traits::fusion_map_range_type fusion_map_range_type;
};

} // namespace geofis

#endif // H93626802_53A7_498E_99B0_56EAA9CCC9C6 
