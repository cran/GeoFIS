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
#include <util/assert.hpp>

#ifndef NDEBUG // DEBUG

#ifdef NO_UTIL_ASSERT_TRHOW // NO_THROW
#include <iostream>
#endif // END NO_THROW

#endif // END DEBUG

#if !defined(NDEBUG) | !defined(NO_UTIL_ASSERT_TRHOW) // DEBUG | THROW
#include <stdexcept>
#include <boost/format.hpp>
#endif // DEBUG | THROW

namespace util {

#if !defined(NDEBUG) | !defined(NO_UTIL_ASSERT_TRHOW) // DEBUG | THROW

std::string assert_message(const char *message, char const *file, unsigned line) {
	return boost::str(boost::format("Assertion \"%1%\" failed in %2% line %3%") % message % file % line);
}

#endif // DEBUG | THROW

#ifndef NO_UTIL_ASSERT_TRHOW // THROW

inline void assert_throw(const std::string &message) {
	throw std::logic_error(message);
}

#endif // END THROW

#ifdef NDEBUG // RELEASE

#ifndef NO_UTIL_ASSERT_TRHOW // THROW

void release_assert(const char *message, char const *file, unsigned line) {
	assert_throw(assert_message(message, file, line));
}

#endif

#else // DEBUG

#ifdef NO_UTIL_ASSERT_TRHOW // NO_THROW
void assert_exit(const std::string &message) {
	std::cerr << message << std::endl;
    std::exit(EXIT_FAILURE);
}
#endif // NO_UTIL_ASSERT_TRHOW

void debug_assert(const char *message, char const *file, unsigned line) {
#ifdef NO_UTIL_ASSERT_TRHOW
	assert_exit(assert_message(message, file, line));
#else // ASSERT_TRHOW
	assert_throw(assert_message(message, file, line));
#endif // NO_UTIL_ASSERT_TRHOW
}

#endif // DEBUG

} // namespace util

