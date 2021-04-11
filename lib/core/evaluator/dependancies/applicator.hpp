// Author: Jordan Randleman -- jordanran199@gmail.com -- applicator.hpp
// => Contains the "execute_application" procedure for the C++ Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_APPLICATOR_HPP_
#define HEIST_SCHEME_CORE_APPLICATOR_HPP_

/******************************************************************************
* TRACING PROCEDURE CALLS
******************************************************************************/

// Prints Debugging Call Trace (see <set-dynamic-call-trace!> primitive)
void output_debug_call_trace(const fcn_type& procedure,const data_vector& arguments,
                             const bool tail_call,    const bool callceing)noexcept{
  // Generate the call signature & Application-Affecting Call States
  auto call_signature = procedure_call_signature(procedure.printable_procedure_name(),arguments);
  const char* in_tail_call = tail_call ? "#t" : "#f";
  const char* using_callce = callceing ? "#t" : "#f";
  // Generate colors for truth-values (Green=#t, Red=#f) of Call States
  const auto tail_c_color = tail_call ? AFMT_32 : AFMT_31;
  const auto callce_color = callceing ? AFMT_32 : AFMT_31;
  // Output Trace Data
  FILE* outs = noexcept_get_current_output_port();
  fprintf(outs,
          "%s%s#<CALL-TRACE>%s Tail-Call: %s%s%s, Call/ce: %s%s%s %s]=>%s %s%s\n",
          HEIST_AFMT(AFMT_01), HEIST_AFMT(AFMT_35), HEIST_AFMT(AFMT_01), 
          HEIST_AFMT(tail_c_color), in_tail_call, HEIST_AFMT(AFMT_01), 
          HEIST_AFMT(callce_color), using_callce, HEIST_AFMT(AFMT_01), 
          HEIST_AFMT(AFMT_35), HEIST_AFMT(AFMT_01), call_signature.c_str(), HEIST_AFMT(AFMT_0));
  fflush(outs);
}


// Prints Call Trace (see <trace> primitive)
void print_call_trace_depth_indentation(const fcn_type& procedure,const bool tail_call=false)noexcept{
  size_type recursive_depth = 0; // default to 0 (level for all prims)
  if(procedure.is_compound()) {
    recursive_depth = procedure.recursive_depth();
    if(recursive_depth && tail_call) --recursive_depth;
  }
  FILE* outs = noexcept_get_current_output_port();
  for(size_type i = 0; i <= recursive_depth; ++i) {
    if(i & 1) fputc(' ',outs);
    else      fputc('|',outs);
  }
  fflush(outs);
}


// Print the current recursive depth as indentation, and the current invocation signature
void output_call_trace_invocation(const fcn_type& procedure, const data_vector& arguments,const bool tail_call=false)noexcept{
  FILE* outs = noexcept_get_current_output_port();
  auto call_signature = procedure_call_signature(procedure.printable_procedure_name(),arguments);
  print_call_trace_depth_indentation(procedure,tail_call);
  fprintf(outs, "%s\n", call_signature.c_str());
  fflush(outs);
}


// Print the current recursive depth as indentation, and the result string
void output_call_trace_result(const fcn_type& procedure, const data& result)noexcept{
  FILE* outs = noexcept_get_current_output_port();
  print_call_trace_depth_indentation(procedure);
  fprintf(outs, "%s\n", result.noexcept_write().c_str());
  fflush(outs);
}


bool tracing_procedure(const string& name)noexcept{
  return !G.TRACED_FUNCTION_NAME.empty() && G.TRACED_FUNCTION_NAME == name;
}

/******************************************************************************
* APPLICATION EXECUTION HELPER FUNCTIONS
******************************************************************************/

// -- STACK TRACE REGISTRATION
void register_call_in_stack_trace(fcn_type& procedure,data_vector& arguments)noexcept{
  if(!G.TRACE_LIMIT) return;
  if(!G.TRACE_ARGS) 
    GLOBALS::STACK_TRACE.push_back(procedure.printable_procedure_name());
  else
    GLOBALS::STACK_TRACE.push_back(procedure_call_signature(procedure.printable_procedure_name(),arguments));
}


// -- APPLYING PRIMITIVE PROCEDURES
data apply_primitive_procedure(data& proc,data_vector&& args,env_type& env,const bool tail_call){
  // Output tracing information as needed
  auto tracing_proc = tracing_procedure(proc.fcn.name);
  if(tracing_proc) output_call_trace_invocation(proc.fcn,args);
  // Provide the environment to primitives applying user-defined procedures
  if(primitive_requires_environment(proc.fcn.prm)) args.push_back(env);
  if(proc.fcn.prm == primitive_APPLY) args.push_back(boolean(tail_call));
  // Extend partially applied args as needed
  if(!proc.fcn.param_instances.empty()) {
    if(args.empty())
      HEIST_THROW_ERR('\''<<proc.fcn.printable_procedure_name()<<" partial procedure didn't receive any arguments!"
        << "\n     Partial Bindings: " << procedure_call_signature(proc.fcn.printable_procedure_name(),proc.fcn.param_instances[0].first));
    args.insert(args.begin(),proc.fcn.param_instances[0].first.begin(),proc.fcn.param_instances[0].first.end());
  }
  auto result = proc.fcn.prm(std::move(args));
  // Clear call from stack strace
  if(!GLOBALS::STACK_TRACE.empty()) GLOBALS::STACK_TRACE.pop_back();
  // Output result's trace as needed
  if(tracing_proc) output_call_trace_result(proc.fcn,result);
  return result;
}


// -- APPLY
// Applies the given procedure, & then reapplies iteratively if at a tail call
data apply_compound_procedure(exe_fcn_t& proc, env_type& extended_env) {
  auto result = proc(extended_env);
  size_type count = 1;
tail_call_recur:
  if(result.is_type(types::exp) && is_tagged_list(result.exp,symconst::tail_call)) { // if tail call
    result = result.exp[1].fcn.bodies[0](result.exp[1].fcn.env);
    ++count;
    goto tail_call_recur;
  }
  // clear calls from stack trace (kept tail calls in trace for debuggability)
  if(count >= GLOBALS::STACK_TRACE.size())
    GLOBALS::STACK_TRACE.clear();
  else
    GLOBALS::STACK_TRACE.erase(GLOBALS::STACK_TRACE.end()-count,GLOBALS::STACK_TRACE.end());
  return result;
}

/******************************************************************************
* APPLICATION EXECUTION
******************************************************************************/

// Analogue to "apply", except no need to analyze the body of compound 
//   procedures (already done). Hence only calls the execution procedure 
//   for the proc's body w/ the extended environment
data execute_application(data& procedure,data_vector&& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
  if(!procedure.is_type(types::fcn))
    HEIST_THROW_ERR("Invalid application of non-procedure "<<HEIST_PROFILE(procedure)<<'!'
      <<HEIST_FCN_ERR(procedure.noexcept_write(),arguments));
  // save call to stack trace output
  register_call_in_stack_trace(procedure.fcn,arguments);
  // output debugger call trace as needed
  if(G.TRACING_ALL_FUNCTION_CALLS)
    output_debug_call_trace(procedure.fcn,arguments,tail_call,procedure.fcn.is_using_dynamic_scope());
  // execute primitive procedure directly
  if(procedure.fcn.is_primitive())
    return apply_primitive_procedure(procedure,std::move(arguments),env,tail_call);
  // compound proc -- create the procedure body's extended environment frame
  exe_fcn_t fcn_body;
  auto extended_env = procedure.fcn.get_extended_environment(arguments,fcn_body,applying_in_cps);
  // splice in current env for dynamic scope as needed
  if(procedure.fcn.is_using_dynamic_scope()) {
    extended_env->parent = env;
  }
  // add the 'self' object iff applying a method
  if(procedure.fcn.self) {
    extended_env->define_variable("self",procedure.fcn.self);
  }
  // confirm max recursive depth hasn't been exceeded
  auto& recursive_depth = procedure.fcn.recursive_depth();
  if(recursive_depth > G.MAX_RECURSION_DEPTH) {
    recursive_depth = 0;
    HEIST_THROW_ERR("Maximum recursion depth of "<<G.MAX_RECURSION_DEPTH<<" exceeded!"
      << HEIST_FCN_ERR(procedure.fcn.printable_procedure_name(), arguments));
  }
  // output tracing information as needed
  auto tracing_proc = tracing_procedure(procedure.fcn.name);
  if(tracing_proc) output_call_trace_invocation(procedure.fcn,arguments,tail_call);
  // store application data & return such back up to the last call if in a tail call
  if(tail_call) {
    data_vector tail_call_signature(2); // {tail-call-tag, proc-body, extended-env}
    tail_call_signature[0] = symconst::tail_call;
    tail_call_signature[1] = fcn_type(extended_env,fcn_body);
    return tail_call_signature;
  }
  // execute compound procedure
  ++recursive_depth;
  auto result = apply_compound_procedure(fcn_body,extended_env);
  --recursive_depth;
  // output result's trace as needed
  if(tracing_proc) output_call_trace_result(procedure.fcn,result);
  return result;
}

// R-value overload
data execute_application(data&& procedure,data_vector&& arguments,env_type& env,const bool tail_call,const bool applying_in_cps){
  return execute_application(procedure,std::move(arguments),env,tail_call,applying_in_cps);
}

#endif