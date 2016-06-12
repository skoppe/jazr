var RxOld = require('../old/rx.lite');
var RxNew = require('../../../dist/rx.lite');
var Benchmark = require('benchmark');

var suite = new Benchmark.Suite;

// Backfill range to get rid of differences
RxOld.range = function (start, count) {
  var scheduler = RxNew.Scheduler.currentThread;
  return new RxNew.AnonymousObservable(function (observer) {
    return scheduler.scheduleRecursive(0, function (i, self) {
      if (i < count) {
        observer.onNext(start + i);
        self(i + 1);
      } else {
        observer.onCompleted();
      }
    });
  });
};
RxNew.range = RxOld.range;

// add tests
suite.add('old', function() {
  RxOld.Observable.zip(
    RxOld.Observable.range(0, 10),
    RxOld.Observable.range(0, 10),
    function (x, y) { return x + y; }
  ).subscribe();
})
.add('new', function() {
  RxNew.Observable.zip(
    RxNew.Observable.range(0, 10),
    RxNew.Observable.range(0, 10),
    function (x, y) { return x + y; }
  ).subscribe();
})
// add listeners
.on('cycle', function(event) {
  console.log(String(event.target));
})
.on('complete', function() {
  console.log('Fastest is ' + this.filter('fastest').pluck('name'));
})
// run async
.run({ 'async': true });
