(function () {
  'use strict';
  /* jshint undef: true, unused: true */
  /* globals QUnit, test, Rx */
  QUnit.module('windowWithTimeOrCount');

  var TestScheduler = Rx.TestScheduler,
      onNext = Rx.ReactiveTest.onNext,
      onError = Rx.ReactiveTest.onError,
      onCompleted = Rx.ReactiveTest.onCompleted,
      subscribe = Rx.ReactiveTest.subscribe;

  test('windowWithTimeOrCount basic', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(205, 1),
      onNext(210, 2),
      onNext(240, 3),
      onNext(280, 4),
      onNext(320, 5),
      onNext(350, 6),
      onNext(370, 7),
      onNext(420, 8),
      onNext(470, 9),
      onCompleted(600));

    var results = scheduler.startScheduler(function () {
      return xs.windowWithTimeOrCount(70, 3, scheduler).map(function (w, i) {
        return w.map(function (x) { return i + ' ' + x; });
      }).mergeAll();
    });

    results.messages.assertEqual(
      onNext(205, '0 1'),
      onNext(210, '0 2'),
      onNext(240, '0 3'),
      onNext(280, '1 4'),
      onNext(320, '2 5'),
      onNext(350, '2 6'),
      onNext(370, '2 7'),
      onNext(420, '3 8'),
      onNext(470, '4 9'),
      onCompleted(600));

    xs.subscriptions.assertEqual(
      subscribe(200, 600));
  });

  test('windowWithTimeOrCount Error', function () {
    var error = new Error();

    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(205, 1),
      onNext(210, 2),
      onNext(240, 3),
      onNext(280, 4),
      onNext(320, 5),
      onNext(350, 6),
      onNext(370, 7),
      onNext(420, 8),
      onNext(470, 9),
      onError(600, error));

    var results = scheduler.startScheduler(function () {
      return xs.windowWithTimeOrCount(70, 3, scheduler).map(function (w, i) {
        return w.map(function (x) { return i + ' ' + x; });
      }).mergeAll();
    });

    results.messages.assertEqual(
      onNext(205, '0 1'),
      onNext(210, '0 2'),
      onNext(240, '0 3'),
      onNext(280, '1 4'),
      onNext(320, '2 5'),
      onNext(350, '2 6'),
      onNext(370, '2 7'),
      onNext(420, '3 8'),
      onNext(470, '4 9'),
      onError(600, error));

    xs.subscriptions.assertEqual(
      subscribe(200, 600));
  });

  test('windowWithTimeOrCount disposed', function () {
    var scheduler = new TestScheduler();

    var xs = scheduler.createHotObservable(
      onNext(205, 1),
      onNext(210, 2),
      onNext(240, 3),
      onNext(280, 4),
      onNext(320, 5),
      onNext(350, 6),
      onNext(370, 7),
      onNext(420, 8),
      onNext(470, 9),
      onCompleted(600));

    var results = scheduler.startScheduler(function () {
      return xs.windowWithTimeOrCount(70, 3, scheduler).map(function (w, i) {
        return w.map(function (x) { return i + ' ' + x; });
      }).mergeAll();
    }, { disposed: 370 });

    results.messages.assertEqual(
      onNext(205, '0 1'),
      onNext(210, '0 2'),
      onNext(240, '0 3'),
      onNext(280, '1 4'),
      onNext(320, '2 5'),
      onNext(350, '2 6'),
      onNext(370, '2 7'));

    xs.subscriptions.assertEqual(
      subscribe(200, 370));
  });

}());
