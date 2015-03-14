package com.oomagnitude.rx

import java.util.concurrent.ExecutorService

import rx.lang.scala.Observable

import scala.concurrent._

object Observables {

  /**
   * Creates observables
   * @tparam A the type of object that is the parameter of the task run in the observable
   * @tparam T the type returned by the observable
   */
  trait ObservableFactory[A, T] {

    /**
     * Create an observer that runs all tasks in parallel and completes when all tasks are completed
     * @param as collection of all inputs to the tasks. They should all be runnable in parallel.
     * @param f the function that is executed for a task
     * @return the observable that completes when all tasks are completed
     */
    def parallel(as: Iterable[A], f: A => T): Observable[T]
  }

  /**
   * A factory that can create Observables that execute tasks in a thread pool. The input to the factory is a batch
   * of items to perform a function on asynchronously. Each function call may run on a different thread. The result is
   * an observable that emits the function return values, one at a time, and then is completed when all functions are
   * complete.
   *
   * @param executorService the ExecutorService for the thread pool
   * @tparam A the type of input to the function that does the work
   * @tparam T the type of output of the function that is run asynchronously
   * @return a factory capable of creating observables out of a collection and a function
   */
  class ThreadPoolObservableFactory[A, T](executorService: ExecutorService) extends ObservableFactory[A, T] {
    override def parallel(as: Iterable[A], f: (A) => T): Observable[T] = {
      // This is the context in which futures will complete
      implicit val executionContext = ExecutionContext.fromExecutorService(executorService)

      Observable(subscriber => {
        Future.sequence(as.map { a => Future { subscriber.onNext(f(a)) }})
          .onSuccess { case _ => subscriber.onCompleted() }
      })
    }
  }

  /**
   * Creates a blocking observable factory where all operations will block the calling thread
   *
   * @tparam A the type of input to the function that does the work
   * @tparam T the type of output of the function that is run asynchronously
   * @return a factory capable of creating observables out of a collection and a function
   */
  class BlockingObservableFactory[A, T] extends ObservableFactory[A, T] {
     override def parallel(as: Iterable[A], f: (A) => T): Observable[T] =
       Observable.just(as.map(a => f(a)).toSeq: _*)
  }
}
