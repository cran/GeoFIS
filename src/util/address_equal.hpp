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
#ifndef ADDRESS_EQUAL_HPP_
#define ADDRESS_EQUAL_HPP_

#include <boost/ref.hpp>
#include <boost/bind/bind.hpp>
#include <boost/utility/addressof.hpp>

namespace util {

template <class T> struct address_equal_to {

	bool operator()(const T &lhs, const T &rhs) const {
		return boost::addressof(lhs) == boost::addressof(rhs);
	}
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <class T> inline bool address_equal(const T &lhs, const T &rhs) {
	return address_equal_to<T>()(lhs, rhs);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <class T> inline auto address_equal(const T &value) {
    return boost::bind<bool>(util::address_equal_to<T>(), boost::placeholders::_1, boost::cref(value));
}

} // namespace util

#endif /* ADDRESS_EQUAL_HPP_ */
