# Task Manager

The implementation is done in Scala. It uses SBT as built tool (https://www.scala-sbt.org/)

## To compile and run the unit test

```shell
$ sbt compile
[info] welcome to sbt 1.3.12 (AdoptOpenJDK Java 11.0.2)
[info] loading project definition from /Users/philippe/Development/Test/task_manager/project
[info] loading settings for project task_manager from build.sbt ...
[info] set current project to iptiQTaskManager (in build file:/Users/philippe/Development/Test/task_manager/)
[info] Executing in batch mode. For better performance use sbt's shell
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[info] Compiling 5 Scala sources to /Users/philippe/Development/Test/task_manager/target/scala-2.13/classes ...
[success] Total time: 6 s, completed 8 Jun 2020, 10:14:51
```

```shell
$ sbt test
[info] welcome to sbt 1.3.12 (AdoptOpenJDK Java 11.0.2)
[info] loading project definition from /Users/philippe/Development/Test/task_manager/project
[info] loading settings for project task_manager from build.sbt ...
[info] set current project to iptiQTaskManager (in build file:/Users/philippe/Development/Test/task_manager/)
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[warn] insecure HTTP request is deprecated 'http://repo.artima.com/releases'; switch to HTTPS or opt-in as ("Artima Maven Repository" at "http://repo.artima.com/releases").withAllowInsecureProtocol(true)
[info] Compiling 2 Scala sources to /Users/philippe/Development/Test/task_manager/target/scala-2.13/test-classes ...
Process(-1297377596, ProcessPriority(MEDIUM)) is killed
Process(-1270759229, ProcessPriority(LOW)) is killed
Process(1140624149, ProcessPriority(HIGH)) is killed
Process(-814509209, ProcessPriority(MEDIUM)) is killed
Process(990080874, ProcessPriority(MEDIUM)) is killed
Process(1673638274, ProcessPriority(LOW)) is killed
Process(-1800692573, ProcessPriority(HIGH)) is killed
[info] ProcessPriorityTest:
[info] - Process priority should have an order
[info] TaskManagerTest:
[info] - TaskManager has a capacity limit
[info] - TaskManager can list processes
[info] - TaskManager can kill processes
[info] - Killing/Listing processes on TestManager with empty capacity do nothing
[info] - TaskManager with a negative capacity is full
[info] - TaskManager with a 0 capacity is full
[info] Run completed in 435 milliseconds.
[info] Total number of tests run: 7
[info] Suites: completed 2, aborted 0
[info] Tests: succeeded 7, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 6 s, completed 8 Jun 2020, 10:15:53
```