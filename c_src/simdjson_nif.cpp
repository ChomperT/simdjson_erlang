#include <cstring>
#include <string.h>
#include <erl_nif.h>
#include <vector>

#include "simdjson.h"
using namespace simdjson;

std::unordered_map<unsigned long, dom::document *> doms;

static ERL_NIF_TERM makeErl(ErlNifEnv *env, simdjson::dom::element &elm) {
	switch(elm.type()) {
		case dom::element_type::ARRAY: {
			std::vector<ERL_NIF_TERM> cs;
			for(dom::element c : dom::array(elm))
				cs.push_back(makeErl(env, c));
			return enif_make_list_from_array(env, cs.data(), cs.size());
		}
		case dom::element_type::OBJECT: {
			std::vector<ERL_NIF_TERM> ks;
			std::vector<ERL_NIF_TERM> vs;
			for(auto field : dom::object(elm)) {
				ErlNifBinary bin;
				std::string_view str = field.key;
				enif_alloc_binary(str.length(), &bin);
				memcpy(bin.data, str.data(), str.length());
				auto k = enif_make_binary(env, &bin);	
				ks.push_back(k);
				vs.push_back(makeErl(env, field.value));
				//vs.push_back(enif_make_tuple2(env, k, makeErl(env, field.value)));
			}
			auto m = enif_make_new_map(env);
			enif_make_map_from_arrays(env, ks.data(), vs.data(), ks.size(), &m);
			//auto m = enif_make_list_from_array(env, vs.data(), vs.size());
			return m;
		}
		case dom::element_type::STRING: {
			ErlNifBinary bin;
			std::string_view str = elm;
			enif_alloc_binary(str.length(), &bin);
			memcpy(bin.data, str.data(), str.length());
			return enif_make_binary(env, &bin);	
		}
		case dom::element_type::INT64:
			return enif_make_long(env, elm);
		case dom::element_type::UINT64:
			return enif_make_ulong(env, elm);
		case dom::element_type::DOUBLE:
			return enif_make_double(env, elm);
		case dom::element_type::BOOL:
			return enif_make_atom(env, elm.get<bool>() == true ? "true" : "false");
		case dom::element_type::NULL_VALUE:
			return enif_make_atom(env, "null");
		default:
			return enif_make_atom(env, "null");
	}
}

static ERL_NIF_TERM decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifBinary res;
	if(!enif_inspect_binary(env, argv[0], &res) &&
		!enif_inspect_iolist_as_binary(env, argv[0], &res))
		return enif_make_badarg(env);
	const char *data = (const char *)res.data;

	try {
		simdjson::dom::parser parser;
		simdjson::dom::element elm = parser.parse(data, res.size);
		auto obj = makeErl(env, elm);
		return obj;
	} catch(simdjson::simdjson_error &error) {
		auto msg = enif_make_string(env, error.what(), ERL_NIF_LATIN1);
		enif_raise_exception(env, msg);
		return msg;
	}
}

static ERL_NIF_TERM lazy_decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifBinary res;
	if(!enif_inspect_binary(env, argv[0], &res) &&
		!enif_inspect_iolist_as_binary(env, argv[0], &res))
		return enif_make_badarg(env);
	const char *data = (const char *)res.data;

	try {
		simdjson::dom::parser parser;
		parser.parse(data, res.size);
		unsigned long id = lrand48();
		do {
			if(doms.find(id) == doms.end())
				break;
			id = lrand48();
		} while(1);
		doms[id] = new dom::document(std::move(parser.doc));
		return enif_make_ulong(env, id);
	} catch(simdjson::simdjson_error &error) {
		auto msg = enif_make_string(env, error.what(), ERL_NIF_LATIN1);
		enif_raise_exception(env, msg);
		return msg;
	}
}

static ERL_NIF_TERM at(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifBinary res;
	if(!enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	if(!enif_inspect_binary(env, argv[1], &res) &&
		!enif_inspect_iolist_as_binary(env, argv[1], &res))
		return enif_make_badarg(env);
	try {
		ulong id = 0;
		enif_get_ulong(env, argv[0], &id);
		auto dom = doms.find(id);
		if(dom == doms.end()) {
			auto invalid = enif_make_atom(env, "invalid_ref");
			enif_raise_exception(env, invalid);
			return invalid;
		} else {
			auto path = std::string_view((char *)res.data, res.size);
			dom::document *root = dom->second;
			dom::element elm = root->root().at(path);
			return makeErl(env, elm);
		}
	} catch(simdjson::simdjson_error &error) {
		auto msg = enif_make_string(env, error.what(), ERL_NIF_LATIN1);
		enif_raise_exception(env, msg);
		return msg;
	}
}

static ERL_NIF_TERM erase(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	if(!enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	ulong id;
	enif_get_ulong(env, argv[0], &id);
	if(doms.find(id) == doms.end()) {
		auto invalid = enif_make_atom(env, "invalid_ref");
		enif_raise_exception(env, invalid);
		return invalid;
	} else {
		auto v = doms[id];
		doms.erase(id);
		delete v;
		return enif_make_atom(env, "ok");
	}
}

static ERL_NIF_TERM erase_all(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	for(auto p : doms)
		delete p.second;
	doms.clear();
	return enif_make_atom(env, "ok");
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info) {
	srand48(time(0));
	return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
	doms.clear();
	return 0;
}

static void unload(ErlNifEnv *env, void *p) {
	doms.clear();
}

static ErlNifFunc funcs[] = {
	{"decode", 1, decode, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"lazy_decode", 1, lazy_decode, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"at", 2, at, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"erase", 1, erase, ERL_NIF_DIRTY_JOB_CPU_BOUND},
	{"erase_all", 0, erase_all, ERL_NIF_DIRTY_JOB_CPU_BOUND},
};
ERL_NIF_INIT(simdjson, funcs, load, 0, upgrade, unload);
