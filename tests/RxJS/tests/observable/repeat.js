(function () {
  'use strict';
  /* jshint undef: true, unused: true */
  /* globals QUnit, test, Rx, raises */

  QUnit.module('Repeat');

  var Observable = Rx.Observable,
      TestScheduler = Rx.TestScheduler,
      onNext = Rx.ReactiveTest.onNext,
      onError = Rx.ReactiveTest.onError,
      onCompleted = Rx.ReactiveTest.onCompleted,
      subscribe = Rx.ReactiveTest.subscribe;

  test('repeat value count zero', function () {
    var scheduler = new TestScheduler();

    var results = scheduler.startScheduler(function () {
      return Observable.repeat(42, 0, scheduler);
    });

    results.messages.assertEqual(
      onCompleted(201)
    );
  });

  test('repeat value count one', function () {
    var scheduler = new TestScheduler();

    var results = scheduler.startScheduler(function () {
      return Observable.repeat(42, 1, scheduler);
    });

    results.messages.assertEqual(
      onNext(201, 42),
      onCompleted(201)
    );
  });

  test('repeat value count ten', function () {
    var scheduler = new TestScheduler();

    var results = scheduler.startScheduler(function () {
      return Observable.repeat(42, 10, scheduler);
    });

    results.messages.assertEqual(
      onNext(201, 42),
      onNext(202, 42),
      onNext(203, 42),
      onNext(204, 42),
      onNext(205, 42),
      onNext(206, 42),
      onNext(207, 42),
      onNext(208, 42),
      onNext(209, 42),
      onNext(210, 42),
      onCompleted(210)
    );
  });

  test('repeat value count dispose', function () {
    var scheduler = new TestScheduler();

    var results = scheduler.startScheduler(function () {
      return Observable.repeat(42, 10, scheduler);
    }, { disposed: 207 });

    results.messages.assertEqual(
      onNext(201, 42),
      onNext(202, 42),
      onNext(203, 42),
      onNext(204, 42),
      onNext(205, 42),
      onNext(206, 42)
    );
  });

  test('repeat value', function () {
    var scheduler = new TestScheduler();

    var results = scheduler.startScheduler(function () {
      return Observable.repeat(42, -1, scheduler);
    }, { disposed: 207 });

    results.messages.assertEqual(
      onNext(201, 42),
      onNext(202, 42),
      onNext(203, 42),
      onNext(204, 42),
      onNext(205, 42),
      onNext(206, 42)
    );
  });

  function noop() { }

  test('repeat Observable basic', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(100, 1),
      onNext(150, 2),
      onNext(200, 3),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return xs.repeat();
    });

    results.messages.assertEqual(
      onNext(300, 1),
      onNext(350, 2),
      onNext(400, 3),
      onNext(550, 1),
      onNext(600, 2),
      onNext(650, 3),
      onNext(800, 1),
      onNext(850, 2),
      onNext(900, 3));

    xs.subscriptions.assertEqual(
      subscribe(200, 450),
      subscribe(450, 700),
      subscribe(700, 950),
      subscribe(950, 1000));
  });

  test('repeat Observable infinite', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(100, 1),
      onNext(150, 2),
      onNext(200, 3));

    var results = scheduler.startScheduler(function () {
      return xs.repeat();
    });

    results.messages.assertEqual(
      onNext(300, 1),
      onNext(350, 2),
      onNext(400, 3));

    xs.subscriptions.assertEqual(
      subscribe(200, 1000));
  });

  test('Repeat Observable error', function () {
    var scheduler = new TestScheduler();

    var error = new Error();

    var xs = scheduler.createColdObservable(
      onNext(100, 1),
      onNext(150, 2),
      onNext(200, 3),
      onError(250, error));

    var results = scheduler.startScheduler(function () {
      return xs.repeat();
    });

    results.messages.assertEqual(
      onNext(300, 1),
      onNext(350, 2),
      onNext(400, 3),
      onError(450, error));

    xs.subscriptions.assertEqual(
      subscribe(200, 450));
  });

  test('Repeat Observable throws', function () {
    var scheduler1 = new TestScheduler();
    var xs = Observable.just(1, scheduler1).repeat();

    xs.subscribe(function () {
      throw new Error();
    });

    raises(function () {
      scheduler1.start();
    });

    var scheduler2 = new TestScheduler();
    var ys = Observable['throw'](new Error(), scheduler2).repeat();

    ys.subscribe(noop, function () {
      throw new Error();
    });

    raises(function () {
      scheduler2.start();
    });

    var scheduler3 = new TestScheduler();
    var zs = Observable.just(1, scheduler3).repeat();

    var d = zs.subscribe(noop, noop, function () { throw new Error(); });

    scheduler3.scheduleAbsolute(null, 210, function () {
      d.dispose();
    });

    scheduler3.start();

    var xss = Observable.create(function () { throw new Error(); }).repeat();

    raises(function () {
      xss.subscribe();
    });
  });

  test('Repeat Observable repeat count Basic', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(5, 1),
      onNext(10, 2),
      onNext(15, 3),
      onCompleted(20));

    var results = scheduler.startScheduler(function () {
      return xs.repeat(3);
    });

    results.messages.assertEqual(
      onNext(205, 1),
      onNext(210, 2),
      onNext(215, 3),
      onNext(225, 1),
      onNext(230, 2),
      onNext(235, 3),
      onNext(245, 1),
      onNext(250, 2),
      onNext(255, 3),
      onCompleted(260));

    xs.subscriptions.assertEqual(
      subscribe(200, 220),
      subscribe(220, 240),
      subscribe(240, 260));
  });

  test('Repeat Observable repeat count dispose', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(5, 1),
      onNext(10, 2),
      onNext(15, 3),
      onCompleted(20));

    var results = scheduler.startScheduler(function () {
      return xs.repeat(3);
    }, { disposed: 231 });

    results.messages.assertEqual(
      onNext(205, 1),
      onNext(210, 2),
      onNext(215, 3),
      onNext(225, 1),
      onNext(230, 2));

    xs.subscriptions.assertEqual(
      subscribe(200, 220),
      subscribe(220, 231));
  });

  test('repeat Observable repeat count infinite', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(100, 1),
      onNext(150, 2),
      onNext(200, 3)
    );

    var results = scheduler.startScheduler(function () {
      return xs.repeat(3);
    });

    results.messages.assertEqual(
      onNext(300, 1),
      onNext(350, 2),
      onNext(400, 3)
    );

    xs.subscriptions.assertEqual(
      subscribe(200, 1000)
    );
  });

  test('repeat Observable repeat count error', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var xs = scheduler.createColdObservable(
      onNext(100, 1),
      onNext(150, 2),
      onNext(200, 3),
      onError(250, error));

    var results = scheduler.startScheduler(function () {
      return xs.repeat(3);
    });

    results.messages.assertEqual(
      onNext(300, 1),
      onNext(350, 2),
      onNext(400, 3),
      onError(450, error));

    xs.subscriptions.assertEqual(
      subscribe(200, 450));
  });

  test('repeat Observable repeat count throws', function () {
    var scheduler1 = new TestScheduler();
    var xs = Observable.just(1, scheduler1).repeat(3);

    xs.subscribe(function () { throw new Error(); });

    raises(function () {
      return scheduler1.start();
    });

    var scheduler2 = new TestScheduler();
    var ys = Observable['throw'](new Error(), scheduler2).repeat(3);

    ys.subscribe(noop, function () { throw new Error(); });

    raises(function () {
      scheduler2.start();
    });

    var scheduler3 = new TestScheduler();
    var zs = Observable.just(1, scheduler3).repeat(100);

    var d = zs.subscribe(noop, noop, function () { throw new Error(); });

    scheduler3.scheduleAbsolute(null, 10, function () {
      d.dispose();
    });

    scheduler3.start();

    var xss = Observable.create(function () { throw new Error(); }).repeat(3);

    raises(function () {
      xss.subscribe();
    });
  });

}());
