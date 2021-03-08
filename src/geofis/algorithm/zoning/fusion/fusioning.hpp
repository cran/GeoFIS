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
#ifndef FUSIONING_HPP_
#define FUSIONING_HPP_

#include <vector>
#include <boost/variant.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <util/range/assign.hpp>
#include <geofis/algorithm/zoning/fusion/aggregation/aggregation_adaptor.hpp>
#include <geofis/algorithm/zoning/fusion/map/fusion_map_range.hpp>
#include <geofis/algorithm/zoning/pair/zone_pair_id_comparator.hpp>

#include <util/type.hpp>

//#include <adobe/timer.hpp>

namespace geofis {

template <class FusioningTraits> class fusioning {

	typedef typename FusioningTraits::aggregation_type 					aggregation_type;
	typedef typename FusioningTraits::zone_distance_type 				zone_distance_type;
	typedef typename FusioningTraits::multidimensional_distance_type	multidimensional_distance_type;
	typedef typename FusioningTraits::attribute_distance_type 			attribute_distance_type;
	typedef typename FusioningTraits::feature_distance_type 			feature_distance_type;
	typedef typename FusioningTraits::feature_normalization_type 		feature_normalization_type;
	typedef typename FusioningTraits::zone_pair_type 					zone_pair_type;
	typedef typename FusioningTraits::neighbor_to_zone_pair_type 		neighbor_to_zone_pair_type;
	typedef typename FusioningTraits::zone_fusion_type 					zone_fusion_type;
	typedef typename FusioningTraits::zone_pair_updater_type 			zone_pair_updater_type;
	typedef aggregation_adaptor<aggregation_type> 						aggregation_adaptor_type;
	typedef std::vector<attribute_distance_type> 						attribute_distance_container_type;
	typedef std::list<zone_pair_type> 									zone_pair_container_type;
	typedef std::list<zone_fusion_type> 								zone_fusion_container_type;
	typedef typename boost::sub_range<zone_fusion_container_type> 		zone_fusion_range_type;
	typedef typename boost::range_iterator<zone_pair_container_type>::type zone_pair_iterator_type;
	typedef std::list<zone_pair_iterator_type> zone_pair_iterator_container_type;

	typedef typename fusion_map_range_traits<zone_fusion_container_type>::fusion_map_range_type fusion_map_range_type;

public:
	//fusioning() : neighbor_to_zone_pair(zone_distance, feature_distance) {}

	template <class FeatureRange, class NeighborRange> void compute(FeatureRange &features, NeighborRange &neighbors) {
		//feature_distance = make_feature_distance<feature_distance_type>(multidimensional_distance, attribute_distances);
		feature_distance = make_feature_distance<feature_distance_type>(multidimensional_distance, attribute_distances);
		//neighbor_to_zone_pair = neighbor_to_zone_pair_type(zone_distance, feature_distance);
		//compute(features, neighbors/*, feature_distance*/);
		zone_fusions.clear();
		feature_normalization_type feature_normalization(feature_normalization_type::initialize(features));
		//std::cout << "feature_normalization" << std::endl;
		feature_normalization.normalize(features);
		//std::cout << "feature_normalization.normalize" << std::endl;
		initialize_zone_pairs_with_neighbors(neighbors/*, feature_distance*/);
		//std::cout << "initialize_zone_pairs_with_neighbors" << std::endl;
		aggregate_zone_pairs(zone_pair_updater_type(feature_distance));
		//std::cout << "aggregate_zone_pairs" << std::endl;
		//feature_normalization.unnormalize(features);
		//std::cout << "feature_normalization.unnormalize" << std::endl;
	}

	void release() {
		zone_pairs.clear();
		zone_fusions.clear();
	}

	void set_aggregation(const aggregation_type &aggregation) {
		this->aggregation = aggregation;
		release();
	}

	template <BOOST_VARIANT_ENUM_PARAMS(class T)> void set_aggregation(const boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> &aggregation) {
		set_aggregation(boost::get<aggregation_type>(aggregation));
	}

	void set_zone_distance(const zone_distance_type &zone_distance) {
		this->zone_distance = zone_distance;
		release();
	}

	template <BOOST_VARIANT_ENUM_PARAMS(class T)> void set_zone_distance(const boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> &zone_distance) {
		set_zone_distance(boost::get<zone_distance_type>(zone_distance));
	}

	void set_multidimensional_distance(const multidimensional_distance_type &multidimensional_distance) {
		this->multidimensional_distance = multidimensional_distance;
		release();
	}

	template <BOOST_VARIANT_ENUM_PARAMS(class T)> void set_multidimensional_distance(const boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> &multidimensional_distance) {
		set_multidimensional_distance(boost::get<multidimensional_distance_type>(multidimensional_distance));
	}

	template <class AttributeDistanceRange> void set_attribute_distances(const AttributeDistanceRange &attribute_distances) {
//		std::cout << "set_attribute_distances \n";
		util::assign(this->attribute_distances, boost::adaptors::transform(attribute_distances, attribute_distance_getter()));
		//util::assign(this->attribute_distances, attribute_distances);
		release();
	}

	/*zone_fusion_range_type get_zone_fusions() {
		return zone_fusions;
	}*/

	size_t get_fusion_size() const {
		return zone_fusions.size();
	}

	template <class ZoneRange> fusion_map_range_type get_fusion_maps(ZoneRange &zones, size_t begin = 0, bool compute_zones = false) {
		return make_fusion_map_range(zone_fusions, begin, zone_fusions.size(), util::make_ref_range(zones), compute_zones);
	}

private:
	aggregation_adaptor_type aggregation;
	zone_distance_type zone_distance;
	multidimensional_distance_type multidimensional_distance;
	attribute_distance_container_type attribute_distances;
	feature_distance_type feature_distance;
	//neighbor_to_zone_pair_type neighbor_to_zone_pair;
	zone_pair_container_type zone_pairs;
	zone_fusion_container_type zone_fusions;

	struct attribute_distance_getter {

		//typedef typename boost::add_reference<const attribute_distance_type>::type result_type;
		typedef attribute_distance_type result_type;

		result_type operator()(const attribute_distance_type &attribute_distance) const {
			return attribute_distance;
		}

		template <BOOST_VARIANT_ENUM_PARAMS(class T)> result_type operator()(const boost::variant<BOOST_VARIANT_ENUM_PARAMS(T)> &attribute_distance) const {
			return boost::get<attribute_distance_type>(attribute_distance);
		}
	};

/*	template <class FeatureRange, class NeighborRange> void compute(FeatureRange &features, NeighborRange &neighbors, const feature_distance_type &feature_distance) {
		zone_fusions.clear();
		feature_normalization_type feature_normalization(feature_normalization_type::initialize(features));
		std::cout << "feature_normalization" << std::endl;
		feature_normalization.normalize(features);
		std::cout << "feature_normalization.normalize" << std::endl;
		initialize_zone_pairs_with_neighbors(neighbors, feature_distance);
		std::cout << "initialize_zone_pairs_with_neighbors" << std::endl;
		aggregate_zone_pairs(zone_pair_updater_type(feature_distance));
		std::cout << "aggregate_zone_pairs" << std::endl;
		feature_normalization.unnormalize(features);
		std::cout << "feature_normalization.unnormalize" << std::endl;
	}*/

	template <class NeighborRange> void initialize_zone_pairs_with_neighbors(NeighborRange &neighbors/*, const feature_distance_type &feature_distance*/) {
		//neighbor_to_zone_pair_type neighbor_to_zone_pair(zone_distance, feature_distance);
		util::assign(zone_pairs, boost::adaptors::transform(neighbors, neighbor_to_zone_pair_type(zone_distance, feature_distance)));
	    zone_pairs.sort(zone_pair_id_comparator());
		//std::cout << "fusioning zone_pairs.size() "  << zone_pairs.size() << std::endl;
	}

	void aggregate_zone_pairs(const zone_pair_updater_type &zone_pair_updater) {
//		adobe::timer_t timer;
		while(!zone_pairs.empty()) {
			zone_pair_iterator_container_type zone_pairs_to_merge;
			aggregation(zone_pairs, std::back_inserter(zone_pairs_to_merge));
			while(!zone_pairs_to_merge.empty()) {
				zone_pair_iterator_type zone_pair_to_merge = zone_pairs_to_merge.front();
				zone_pairs_to_merge.pop_front();
				aggregate_zone_pair(zone_pair_to_merge, zone_pair_updater, zone_pairs_to_merge);
			}
		}
//			aggregate_zone_pair(aggregation(zone_pairs), zone_pair_updater);
//		timer.report("fusioning aggregation");
		//std::cout << "fusioning zone_fusions.size() "  << zone_fusions.size() << std::endl;
	}

	template <class ZonePairIterator> void aggregate_zone_pair(const ZonePairIterator &iterator, const zone_pair_updater_type &zone_pair_updater, zone_pair_iterator_container_type &zone_pairs_to_merge) {
		zone_fusions.push_back(zone_fusion_type(*iterator));
		zone_pairs.erase(iterator);
		zone_pair_updater.update_zone_pairs(zone_pairs, zone_fusions.back(), zone_pairs_to_merge);
	}
};

} // namespace geofis

#endif /* FUSIONING_HPP_ */
