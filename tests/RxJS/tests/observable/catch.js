(function () {
  /* jshint undef: true, unused: true */
  /* globals QUnit, test, Rx, ok, equal */

  QUnit.module('catch');

  var Observable = Rx.Observable,
      TestScheduler = Rx.TestScheduler,
      onNext = Rx.ReactiveTest.onNext,
      onError = Rx.ReactiveTest.onError,
      onCompleted = Rx.ReactiveTest.onCompleted;

  test('Catch NoErrors', function () {
    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onCompleted(230));
    var o2 = scheduler.createHotObservable(
      onNext(240, 5),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onCompleted(230));
  });

  test('Catch Never', function () {
    var scheduler = new TestScheduler();

    var o1 = Observable.never();
    var o2 = scheduler.createHotObservable(
      onNext(240, 5),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual();
  });

  test('Catch Empty', function () {
    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onCompleted(230));
    var o2 = scheduler.createHotObservable(
      onNext(240, 5),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onCompleted(230));
  });

  test('Catch Return', function () {
    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onCompleted(230));

    var o2 = scheduler.createHotObservable(
      onNext(240, 5),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onCompleted(230));
  });

  test('Catch Error', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onError(230, error));

    var o2 = scheduler.createHotObservable(
      onNext(240, 5),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onNext(240, 5),
      onCompleted(250));
  });

  test('Catch Error Never', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onError(230, error));

    var o2 = Observable.never();

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3));
  });

  test('Catch Error Error', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onError(230, new Error()));

    var o2 = scheduler.createHotObservable(
      onNext(240, 4),
      onError(250, error));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](o2);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onNext(240, 4),
      onError(250, error)
    );
  });

  test('Catch Multiple', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onError(215, error));

    var o2 = scheduler.createHotObservable(
      onNext(220, 3),
      onError(225, error));

    var o3 = scheduler.createHotObservable(
      onNext(230, 4),
      onCompleted(235));

    var results = scheduler.startScheduler(function () {
      return Observable['catch'](o1, o2, o3);
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onNext(230, 4),
      onCompleted(235)
    );
  });

  test('Catch ErrorSpecific Caught', function () {
    var error = new Error();

    var handlerCalled = false;

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onError(230, error)
    );
    var o2 = scheduler.createHotObservable(
      onNext(240, 4),
      onCompleted(250)
    );

    var results = scheduler.startScheduler(function () {
      return o1['catch'](function () {
        handlerCalled = true;
        return o2;
      });
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onNext(240, 4),
      onCompleted(250)
    );

    ok(handlerCalled);
  });

  test('Catch ErrorSpecific CaughtImmediate', function () {
    var handlerCalled = false;

    var scheduler = new TestScheduler();

    var o2 = scheduler.createHotObservable(
      onNext(240, 4),
      onCompleted(250));

    var results = scheduler.startScheduler(function () {
      return Observable['throw'](new Error())['catch'](function () {
        handlerCalled = true;
        return o2;
      });
    });

    results.messages.assertEqual(
      onNext(240, 4),
      onCompleted(250));

    ok(handlerCalled);
  });

  test('Catch HandlerThrows', function () {
    var error = new Error();

    var error2 = new Error();

    var handlerCalled = false;

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onNext(220, 3),
      onError(230, error));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](function () {
        handlerCalled = true;
        throw error2;
      });
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onError(230, error2));

    ok(handlerCalled);
  });

  test('Catch Nested OuterCatches', function () {
    var error = new Error();

    var firstHandlerCalled = false;
    var secondHandlerCalled = false;

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onError(215, error));

    var o2 = scheduler.createHotObservable(
      onNext(220, 3),
      onCompleted(225));

    var o3 = scheduler.createHotObservable(
      onNext(220, 4),
      onCompleted(225));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](function () {
        firstHandlerCalled = true;
        return o2;
      })['catch'](function () {
        secondHandlerCalled = true;
        return o3;
      });
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onCompleted(225));

    ok(firstHandlerCalled);
    ok(!secondHandlerCalled);
  });

  test('Catch ThrowFromNestedCatch', function () {
    var error = new Error();

    var error2 = new Error();

    var firstHandlerCalled = false;
    var secondHandlerCalled = false;

    var scheduler = new TestScheduler();

    var o1 = scheduler.createHotObservable(
      onNext(150, 1),
      onNext(210, 2),
      onError(215, error));

    var o2 = scheduler.createHotObservable(
      onNext(220, 3),
      onError(225, error2));

    var o3 = scheduler.createHotObservable(
      onNext(230, 4),
      onCompleted(235));

    var results = scheduler.startScheduler(function () {
      return o1['catch'](function (e) {
        firstHandlerCalled = true;
        equal(e, error);
        return o2;
      })['catch'](function (e) {
        secondHandlerCalled = true;
        equal(e, error2);
        return o3;
      });
    });

    results.messages.assertEqual(
      onNext(210, 2),
      onNext(220, 3),
      onNext(230, 4),
      onCompleted(235));

    ok(firstHandlerCalled);
    ok(secondHandlerCalled);
  });

  test('Rx.Observable.catch() does not lose subscription to underlying observable', 12, function () {
    var subscribes = 0,
      unsubscribes = 0,
      tracer = Rx.Observable.create(function () { ++subscribes; return function () { ++unsubscribes; }; }),
      s;

    // Try it without catchError()
    s = tracer.subscribe();
    equal(subscribes, 1, '1 subscribes');
    equal(unsubscribes, 0, '0 unsubscribes');
    s.dispose();
    equal(subscribes, 1, 'After dispose: 1 subscribes');
    equal(unsubscribes, 1, 'After dispose: 1 unsubscribes');

    // Now try again with catchError(Observable):
    subscribes = unsubscribes = 0;
    s = tracer['catch'](Rx.Observable.never()).subscribe();
    equal(subscribes, 1, 'catchError(Observable): 1 subscribes');
    equal(unsubscribes, 0, 'catchError(Observable): 0 unsubscribes');
    s.dispose();
    equal(subscribes, 1, 'catchError(Observable): After dispose: 1 subscribes');
    equal(unsubscribes, 1, 'catchError(Observable): After dispose: 1 unsubscribes');

    // And now try again with catchError(function()):
    subscribes = unsubscribes = 0;
    s = tracer['catch'](function () { return Rx.Observable.never(); }).subscribe();
    equal(subscribes, 1, 'catchError(function): 1 subscribes');
    equal(unsubscribes, 0, 'catchError(function): 0 unsubscribes');
    s.dispose();
    equal(subscribes, 1, 'catchError(function): After dispose: 1 subscribes');
    equal(unsubscribes, 1, 'catchError(function): After dispose: 1 unsubscribes'); // this one FAILS (unsubscribes is 0)
  });

}());
