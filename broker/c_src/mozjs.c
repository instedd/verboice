#include "erl_nif.h"
#include "js/jsapi.h"
#include <stdio.h>

static ErlNifResourceType *js_runtime_type = NULL;
static ErlNifResourceType *js_context_type = NULL;

typedef struct { JSRuntime *rt; } js_runtime;
typedef struct { JSContext *cx; JSObject *scope; } js_context;

static JSClass global_class = {
  "global",                         // name
  JSCLASS_GLOBAL_FLAGS,             // flags
  JS_PropertyStub,                  // addProperty
  JS_PropertyStub,                  // delProperty
  JS_PropertyStub,                  // getProperty
  JS_StrictPropertyStub,            // setProperty
  JS_EnumerateStub,                 // enumerate
  JS_ResolveStub,                   // resolve
  JS_ConvertStub,                   // convert
  JS_FinalizeStub,                  // finalize
  JSCLASS_NO_OPTIONAL_MEMBERS
};


static void gc_js_runtime(ErlNifEnv *env, void *d) {
  js_runtime *rt = (js_runtime *)d;
  if (rt->rt != 0)
    JS_DestroyRuntime(rt->rt);
  rt->rt = 0;
}

static void gc_js_context(ErlNifEnv *env, void *d) {
  js_context *cx = (js_context *)d;
  if (cx->cx != 0)
    JS_DestroyContext(cx->cx);
  cx->cx = 0;
}

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
  js_runtime_type = enif_open_resource_type(env, NULL, "js_runtime_type", gc_js_runtime, ERL_NIF_RT_CREATE, NULL);
  js_context_type = enif_open_resource_type(env, NULL, "js_context_type", gc_js_context, ERL_NIF_RT_CREATE, NULL);

  return 0;
}

static ERL_NIF_TERM new_runtime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  js_runtime *rt;
  ERL_NIF_TERM result;

  rt = enif_alloc_resource(js_runtime_type, sizeof(js_runtime));
  rt->rt = JS_NewRuntime(8L * 1024L * 1024L);

  result = enif_make_resource(env, rt);
  enif_release_resource(rt);

  return result;
}

static ERL_NIF_TERM new_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  js_runtime *rt;
  js_context *cx;
  ERL_NIF_TERM result;

  if (!enif_get_resource(env, argv[0], js_runtime_type, (void **) &rt))
    return enif_make_badarg(env);

  cx = enif_alloc_resource(js_context_type, sizeof(js_context));
  cx->cx = JS_NewContext(rt->rt, 8192);

  JS_SetContextThread(cx->cx);
  JS_BeginRequest(cx->cx);

  JS_SetOptions(cx->cx, JSOPTION_VAROBJFIX);
  JS_SetVersion(cx->cx, JSVERSION_LATEST);

  cx->scope = JS_NewCompartmentAndGlobalObject(cx->cx, &global_class, NULL);
  JS_InitStandardClasses(cx->cx, cx->scope);

  JS_EndRequest(cx->cx);
  JS_ClearContextThread(cx->cx);

  result = enif_make_resource(env, cx);
  enif_release_resource(cx);

  return enif_make_tuple2(env, argv[0], result);
}

static ERL_NIF_TERM js_to_int(ErlNifEnv* env, JSContext *cx, jsval v) {
  return enif_make_int(env, JSVAL_TO_INT(v));
}

static ERL_NIF_TERM js_to_bool(ErlNifEnv* env, JSContext *cx, jsval v) {
  return enif_make_atom(env, JSVAL_TO_BOOLEAN(v) == JS_TRUE ? "true" : "false");
}

static ERL_NIF_TERM js_to_double(ErlNifEnv* env, JSContext *cx, jsval v) {
  return enif_make_double(env, JSVAL_TO_DOUBLE(v));
}

static ERL_NIF_TERM js_to_string(ErlNifEnv* env, JSContext *cx, jsval v) {
  char *value = JS_EncodeString(cx, JSVAL_TO_STRING(v));
  ERL_NIF_TERM result = enif_make_string(env, value, ERL_NIF_LATIN1);
  JS_free(cx, value);
  return result;
}

static ERL_NIF_TERM eval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  const ERL_NIF_TERM *context_arg;
  int context_arg_length;
  js_context *cx;
  char script[1024];
  jsval ret;

  if (!enif_get_tuple(env, argv[0], &context_arg_length, &context_arg))
    return enif_make_badarg(env);

  if (!enif_get_resource(env, context_arg[1], js_context_type, (void **) &cx))
    return enif_make_badarg(env);

  if (enif_get_string(env, argv[1], script, 1024, ERL_NIF_LATIN1) <= 0)
    return enif_make_badarg(env);

  JS_SetContextThread(cx->cx);
  JS_BeginRequest(cx->cx);

  JS_EvaluateScript(cx->cx, cx->scope, script, strlen(script), "<<script>>", 1, &ret);

  JS_EndRequest(cx->cx);
  JS_ClearContextThread(cx->cx);

  if (JSVAL_IS_NULL(ret)) {
    return enif_make_atom(env, "null");
  } else if (JSVAL_IS_BOOLEAN(ret)) {
    return js_to_bool(env, cx->cx, ret);
  } else if (JSVAL_IS_INT(ret)) {
    return js_to_int(env, cx->cx, ret);
  } else if (JSVAL_IS_NUMBER(ret)) {
    return js_to_double(env, cx->cx, ret);
  } else if (JSVAL_IS_STRING(ret)) {
    return js_to_string(env, cx->cx, ret);
  }

  return enif_make_atom(env, "undefined");
}

static ERL_NIF_TERM shutdown(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  JS_ShutDown();
  return enif_make_int(env, 123);
}

static ErlNifFunc nif_funcs[] = {
  {"new_runtime", 0, new_runtime},
  {"new_context", 1, new_context},
  {"shutdown", 0, shutdown},
  {"eval", 2, eval}
};

ERL_NIF_INIT(mozjs, nif_funcs, load, NULL, NULL, NULL)
