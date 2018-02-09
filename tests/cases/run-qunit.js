"use strict";
var system = require('system');

/**
 * Wait until the test condition is true or a timeout occurs. Useful for waiting
 */
function waitFor(testFx, onReady, timeOutMillis) {
    var maxtimeOutMillis = timeOutMillis ? timeOutMillis : 3001, //< Default Max Timout is 3s
        start = new Date().getTime(),
        condition = false,
        state = {interval: 250, timeout: timeOutMillis, noChangeCounter: 0},
        interval = setInterval(function() {
            // if ( (new Date().getTime() - start < maxtimeOutMillis) && !condition ) {
                condition = (typeof(testFx) === "string" ? eval(testFx) : testFx(state)); //< defensive code
            // } else {
                if(condition == 'timeout') {
                    console.log("'waitFor()' timeout");
                    phantom.exit(1);
                } else if (condition) {
                    // Condition fulfilled (timeout and/or condition is 'true')
                    console.log("'waitFor()' finished in " + (new Date().getTime() - start) + "ms.");
                    clearInterval(interval); //< Stop this interval
                }
            // }
        }, 250); //< repeat check every 250ms
};
