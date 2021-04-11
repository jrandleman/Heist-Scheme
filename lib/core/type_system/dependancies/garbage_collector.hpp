// Author: Jordan Randleman -- jordanran199@gmail.com -- garbage_collector.hpp
// => Contains shared_ptr/GC struct for the C++ Heist Scheme Interpreter

//
// The <tgc_ptr> struct below provides reference-counting pointers, and comes 
//   in 2 flavors: cycle-safe, and cycle-unsafe (the latter avoiding GC overhead).
//
// By default it's cycle-safe, BUT passing in 0 for <INIT_TGC_CAPACITY> when 
//   instantiating the struct makes it cycle-unsafe.
//
// The reasonable among you may well wonder why in the world should there be 2 options
//   when <std::shared_ptr> already exists as a standardized means to have cycle-unsafe
//   reference-counting pointers. The reasoning? The establishment of a common interface 
//   among ALL reference-counting pointers (regardless of cycle safety) throughout the
//   entire Heist-Scheme interpreter.
//

#ifndef HEIST_SCHEME_CORE_GARBAGE_COLLECTOR_HPP_
#define HEIST_SCHEME_CORE_GARBAGE_COLLECTOR_HPP_

// GOAL: Combine reference counting w/ "GC" for cycle-safe "shared_ptr"s
// APPROACH: Regular reference counting, w/ any ptr refs > 0 also
//           having an entry in the GC. Once ref = 0, the entry is 
//           rm'd from the GC. GC is freed upon exit via a global dtor.
namespace heist {
  namespace GLOBALS {
    // Avoid <atexit> (may limit capacity to 32 fcns) via a global object dtor
    struct tgc_atexit_t {
      std::vector<void(*)(void)> ATEXIT_FUNCTIONS;
      void operator()(void(*f)(void)) noexcept { ATEXIT_FUNCTIONS.push_back(f); }
      ~tgc_atexit_t() noexcept { for(auto f : ATEXIT_FUNCTIONS) f(); }
    };
    tgc_atexit_t tgc_atexit;
  }

  // TGC Pointer Struct
  template<typename VAL_T,std::size_t INIT_TGC_CAPACITY=32,std::size_t TGC_CAPACITY_SCALAR=2>
  struct tgc_ptr {
    // STATIC TYPED GC INVARIANTS & atexit-FREEING FUNCTION
    using TGC_ENTRY = std::pair<std::size_t*,VAL_T*>; // ref_count/ptr pairs
    static std::size_t TGC_CAP, TGC_LEN;
    static TGC_ENTRY* TYPED_GARBAGE_COLLECTOR;
    static void FREE_TYPED_GARBAGE_COLLECTOR()noexcept{
      TGC_CAP = 0; // SIGNALS GC FREED & DISABLES ALL INDEPENDENT DTORS
      for(std::size_t i = TGC_LEN; i-- > 0;) {
        delete TYPED_GARBAGE_COLLECTOR[i].first;
        delete TYPED_GARBAGE_COLLECTOR[i].second;
        TYPED_GARBAGE_COLLECTOR[i].first = nullptr;
        TYPED_GARBAGE_COLLECTOR[i].second = nullptr;
      }
      delete [] TYPED_GARBAGE_COLLECTOR;
      TYPED_GARBAGE_COLLECTOR = nullptr, TGC_LEN = 0;
    }

    // INVARIANTS
    std::size_t* ref_count = nullptr;
    VAL_T* ptr = nullptr;

    // REGISTER & REMOVE PTR FROM TGC
    void register_in_TGC()noexcept{
      // Verify either NOT storing in GC, OR have a proper scalar
      static_assert(INIT_TGC_CAPACITY == 0 || TGC_CAPACITY_SCALAR > 1, 
                    "Cycle-safe pointers reguire a 'TGC_CAPACITY_SCALAR' > 1!");
      // Place in GC (most common)
      if(TGC_LEN < TGC_CAP) {
        TYPED_GARBAGE_COLLECTOR[TGC_LEN++] = TGC_ENTRY(ref_count,ptr);
      // Init GC & register the its freeing of members
      } else if(!TGC_CAP) {
        TGC_CAP = INIT_TGC_CAPACITY;
        TYPED_GARBAGE_COLLECTOR = new TGC_ENTRY [TGC_CAP];
        GLOBALS::tgc_atexit(FREE_TYPED_GARBAGE_COLLECTOR);
        TYPED_GARBAGE_COLLECTOR[TGC_LEN++] = TGC_ENTRY(ref_count,ptr);
      // Expand GC size if TGC_LEN == TGC_CAP
      } else {
        TGC_CAP *= TGC_CAPACITY_SCALAR;
        TGC_ENTRY* tmp = TYPED_GARBAGE_COLLECTOR;
        TYPED_GARBAGE_COLLECTOR = new TGC_ENTRY [TGC_CAP];
        for(std::size_t i = 0; i < TGC_LEN; ++i)
          TYPED_GARBAGE_COLLECTOR[i] = std::move(tmp[i]);
        delete [] tmp;
        TYPED_GARBAGE_COLLECTOR[TGC_LEN++] = TGC_ENTRY(ref_count,ptr);
      }
    }
    void deregister_in_TGC()const noexcept{
      for(std::size_t i = TGC_LEN; i-- > 0;) // optimize search for short ptr lifetimes
        if(TYPED_GARBAGE_COLLECTOR[i].first == ref_count) {
          TYPED_GARBAGE_COLLECTOR[i] = std::move(TYPED_GARBAGE_COLLECTOR[--TGC_LEN]);
          return;
        }
    }

    // CTORS
    tgc_ptr()noexcept{}
    tgc_ptr(std::nullptr_t)noexcept{}
    tgc_ptr(const VAL_T& obj)noexcept{
      ptr = new VAL_T(obj);
      ref_count = new std::size_t(1);
      if constexpr (INIT_TGC_CAPACITY > 0) register_in_TGC();
    }
    tgc_ptr(VAL_T&& obj)noexcept{
      ptr = new VAL_T(std::move(obj));
      ref_count = new std::size_t(1);
      if constexpr (INIT_TGC_CAPACITY > 0) register_in_TGC();
    }
    tgc_ptr(const tgc_ptr& tgc_p)noexcept{
      if(!tgc_p.use_count()) return;
      ptr = tgc_p.ptr, ref_count = tgc_p.ref_count;
      ++(*ref_count);
    }
    tgc_ptr(tgc_ptr&& tgc_p)noexcept{ // moves don't increase ref_count
      if(!tgc_p.use_count()) return;
      ptr = tgc_p.ptr, ref_count = tgc_p.ref_count;
      tgc_p.ptr = nullptr, tgc_p.ref_count = nullptr;
    }

    // DELETE INNER POINTER (USING REFERENCE COUNTING)
    // NOTE: Never double frees, always safe to call!
    void delete_tgc_ptr()noexcept{
      if(!use_count()) return;
      if(*ref_count > 1) {
        --(*ref_count);
      } else { // last obj
        if constexpr (INIT_TGC_CAPACITY > 0) deregister_in_TGC();
        delete ref_count;
        delete ptr;
      }
      ref_count = nullptr, ptr = nullptr;
    }

    // ASSIGNMENT
    void operator=(const tgc_ptr& tgc_p)noexcept{
      if(this == &tgc_p) return;
      delete_tgc_ptr();
      if(!tgc_p.use_count()) return;
      ptr = tgc_p.ptr, ref_count = tgc_p.ref_count;
      ++(*ref_count);
    }
    void operator=(tgc_ptr&& tgc_p)noexcept{ // moves don't increase ref_count
      if(this == &tgc_p) return;
      delete_tgc_ptr();
      if(!tgc_p.use_count()) return;
      ptr = tgc_p.ptr, ref_count = tgc_p.ref_count;
      tgc_p.ptr = nullptr, tgc_p.ref_count = nullptr;
    }

    // UNDERLYING POINTER ACCESSORS (PRECONDITION: ref_count && *ref_count > 0)
    VAL_T& operator*()noexcept{return *ptr;}
    VAL_T* operator->()noexcept{return ptr;}
    const VAL_T& operator*()const noexcept{return *ptr;}
    const VAL_T* operator->()const noexcept{return ptr;}

    // CYCLE-SAFETY PREDICATE
    bool is_cycle_safe()const noexcept{return INIT_TGC_CAPACITY;}

    // POINTER COMPARISON
    bool operator==(const tgc_ptr& p)const noexcept{return ptr == p.ptr;}
    bool operator!=(const tgc_ptr& p)const noexcept{return ptr != p.ptr;}
    bool operator==(tgc_ptr&& p)const noexcept{return ptr == p.ptr;}
    bool operator!=(tgc_ptr&& p)const noexcept{return ptr != p.ptr;}
    bool operator!()const noexcept{return ptr == nullptr;}
    operator bool()const noexcept{return ptr != nullptr;}

    // REFCOUNT ACCESSOR
    std::size_t use_count()const noexcept{
      if(!ref_count) return 0;
      return *ref_count;
    }

    // DTOR
    ~tgc_ptr()noexcept{ 
      if constexpr (INIT_TGC_CAPACITY > 0) {
        if(TGC_CAP) delete_tgc_ptr(); // TGC not yet invoked
      } else {
        delete_tgc_ptr();
      }
    }
  }; // End of struct tgc_ptr

  // Initialize static members of <tgc_ptr>
  template <typename VAL_T,std::size_t INIT_CAP,std::size_t CAP_SCALAR>
  std::size_t tgc_ptr<VAL_T,INIT_CAP,CAP_SCALAR>::TGC_CAP = 0;
  template <typename VAL_T,std::size_t INIT_CAP,std::size_t CAP_SCALAR>
  std::size_t tgc_ptr<VAL_T,INIT_CAP,CAP_SCALAR>::TGC_LEN = 0;
  template <typename VAL_T,std::size_t INIT_CAP,std::size_t CAP_SCALAR>
  typename tgc_ptr<VAL_T,INIT_CAP,CAP_SCALAR>::TGC_ENTRY* tgc_ptr<VAL_T,INIT_CAP,CAP_SCALAR>::TYPED_GARBAGE_COLLECTOR = nullptr;
} // End of namespace heist
#endif