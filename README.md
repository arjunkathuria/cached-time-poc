# README

This repo is a POC implementation for cached-time.

## The idea

The idea behind cached time that instead of calling the IO action `getCurrentTime` everytime
one needs to get the current time, we can read a timestamp from a location saved somewhere in the app,
like in our appstate.

This saved time in our appstate is updated at regular intervals (configurable by us) by a background thread (the timer-thread).

There was a question of making sure this timer-thread doesn't die. As a basic redundancy mechanism, the concept of a polling-thread
was implemented. This polling thread polls the timer-thread at regular intervals (again, configurable by us) to see if it's still
running or has it completed/died. The polling thread can then take an appropriate action based on that information. We respawn
the timer-thread in this implementation.

## Instructions to run

### The executable program 

The project has an executable, which can be run to see cached-time implementation in action. To run it, use

```
$ cabal run
```

### The benchmarks 

The project comes with a basic benchmarking-suite, which gives us the timings for
    1. the time cost of running `getCurrentTime`.
    2. the time cost of fetching cachedTime from appstate using provided `getCachedTime` abstraction
    3. the time cost of directly reading cachedTime from appstate.
    
To run the benchmarks, please use:

```
$ cabal bench
```

or 

```
$ cabal bench -O2
```

The `-O2` flag is optional.


## Benchmark results:-

The benchmark results from this POC are as follows:

1. Results for running `getCurrentTime` from `time` library. This is what most of our codebase uses currently :-


```
benchmarking getCurrentTime from IO

time                 53.33 ns   (53.21 ns .. 53.45 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 53.18 ns   (53.05 ns .. 53.34 ns)
std dev              480.9 ps   (351.5 ps .. 682.6 ps)


```

2. Results for running `getCachedTime` implementation, 10ms timer-thread updates

```
benchmarking Cached time/Get Cached Time from AppState

Current cached Time: 2023-06-21 12:24:43.569628 UTC
Updated cached Time: 2023-06-21 12:24:48.774355 UTC

Current cached Time: 2023-06-21 12:24:48.774355 UTC
Updated cached Time: 2023-06-21 12:24:48.784602 UTC

Current cached Time: 2023-06-21 12:24:48.784602 UTC
Updated cached Time: 2023-06-21 12:24:48.795323 UTC

Current cached Time: 2023-06-21 12:24:48.795323 UTC
Updated cached Time: 2023-06-21 12:24:48.805532 UTC

time                 6.424 ns   (6.417 ns .. 6.431 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.431 ns   (6.424 ns .. 6.439 ns)
std dev              25.78 ps   (20.81 ps .. 37.80 ps)
```


3. Results for reading `cachedTime` directly from appstate, 10ms timer-thread updates

```
benchmarking Cached time/Read cachedTime Directly

Current cached Time: 2023-06-21 12:24:48.813985 UTC
Updated cached Time: 2023-06-21 12:24:53.894775 UTC

Current cached Time: 2023-06-21 12:24:53.894775 UTC
Updated cached Time: 2023-06-21 12:24:53.90515 UTC

Current cached Time: 2023-06-21 12:24:53.90515 UTC
Updated cached Time: 2023-06-21 12:24:53.915781 UTC

Current cached Time: 2023-06-21 12:24:53.915781 UTC
Updated cached Time: 2023-06-21 12:24:53.926377 UTC

time                 5.165 ns   (5.154 ns .. 5.177 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.163 ns   (5.157 ns .. 5.169 ns)
std dev              18.73 ps   (14.69 ps .. 25.13 ps)
```


## Implementation Details

Cached time is implemented as a timestamp from `getCurrentTime` stored in a mutable `TVar` (a transation variable from STM, a safe shared memeory location). The Appstate is initialized with the current time in the `TVar`. After that, a timer-thread is spawned to updated the time at the mutable variable (using STM transactions) at regular time intervals (configured by us) by writing to the above TVar. There's a polling thread implemented as a redundancy for the timer-thread dying.
The main thread is also explicitly *linked* with the timer thread, any exceptions occuring in the timer-thread are then thrown to the main thread, which, when un-handled, would crash the main thread as well (by design).

### Future scope for implementation details

* Try implementing with `MVars` or raw `IORefs` if `TVars` and the whole transaction mechanism ends up too slow for us in the final form.

* Handle exception thrown by the timer-thread.

* See and evaluate if we need the polling mechanism after handling exceptions thrown from the timer-thread.

* Improve, remove or decide on new redundancy and consistency measures.
