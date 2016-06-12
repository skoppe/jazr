(function () {
  'use strict';
  /* jshint undef: true, unused: true */
  /* globals QUnit, test, Rx */

  QUnit.module('partition');

  var TestScheduler = Rx.TestScheduler,
    ReactiveTest = Rx.ReactiveTest,
    onNext = ReactiveTest.onNext,
    onError = ReactiveTest.onError,
    onCompleted = ReactiveTest.onCompleted,
    subscribe = ReactiveTest.subscribe;

  function isEven(num) {
    return +num % 2 === 0;
  }

  test('partition empty', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onCompleted(210)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onCompleted(210)
    );

    results2.messages.assertEqual(
      onCompleted(210)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 210),
      subscribe(200, 210)
    );
  });

  test('partition single', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onCompleted(220)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4),
      onCompleted(220)
    );

    results2.messages.assertEqual(
      onCompleted(220)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 220),
      subscribe(200, 220)
    );
  });

  test('partition each', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onNext(220, 3),
      onCompleted(230)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4),
      onCompleted(230)
    );

    results2.messages.assertEqual(
      onNext(220, 3),
      onCompleted(230)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 230),
      subscribe(200, 230)
    );
  });

  test('partition completed', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onNext(240, 3),
      onNext(290, 2),
      onNext(350, 1),
      onCompleted(360)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4),
      onNext(290, 2),
      onCompleted(360)
    );

    results2.messages.assertEqual(
      onNext(240, 3),
      onNext(350, 1),
      onCompleted(360)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 360),
      subscribe(200, 360)
    );
  });

  test('partition not completed', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onNext(240, 3),
      onNext(290, 2),
      onNext(350, 1)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4),
      onNext(290, 2)
    );

    results2.messages.assertEqual(
      onNext(240, 3),
      onNext(350, 1)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 1000),
      subscribe(200, 1000)
    );
  });

  test('partition error', function () {
    var error = new Error();
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onNext(240, 3),
      onError(290, error),
      onNext(350, 1),
      onCompleted(360)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.disposed, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4),
      onError(290, error)
    );

    results2.messages.assertEqual(
      onNext(240, 3),
      onError(290, error)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 290),
      subscribe(200, 290)
    );
  });

  test('partition disposed', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(180, 5),
      onNext(210, 4),
      onNext(240, 3),
      onNext(290, 2),
      onNext(350, 1),
      onCompleted(360)
    );

    var observables,
      subscription1,
      subscription2,
      results1 = scheduler.createObserver(),
      results2 = scheduler.createObserver();

    scheduler.scheduleAbsolute(null, ReactiveTest.created, function () {
      observables = xs.partition(isEven);
    });

    scheduler.scheduleAbsolute(null, ReactiveTest.subscribed, function () {
      subscription1 = observables[0].subscribe(results1);
      subscription2 = observables[1].subscribe(results2);
    });

    scheduler.scheduleAbsolute(null, 280, function () {
      subscription1.dispose();
      subscription2.dispose();
    });

    scheduler.start();

    results1.messages.assertEqual(
      onNext(210, 4)
    );

    results2.messages.assertEqual(
      onNext(240, 3)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 280),
      subscribe(200, 280)
    );
  });

}());
