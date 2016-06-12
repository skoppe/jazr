var RxOld = require('../old/rx.lite');
var RxNew = require('../../../dist/rx.lite');
var Benchmark = require('benchmark');

var suite = new Benchmark.Suite;

var args = [];
for (var i = 0; i < 25; i++) { args.push(i); }

// add tests
suite.add('old', function() {
  RxOld.Observable.of.apply(null, args).subscribe();
})
.add('new', function() {
  RxNew.Observable.of.apply(null, args).subscribe();
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
