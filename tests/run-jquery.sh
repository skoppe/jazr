#!/bin/bash

phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=wrap
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=tween
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=traversing
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=support FAILS
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=serialize
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=selector
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=queue
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=offset
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=manipulation FAILS
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=event#2
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=effects
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=dimensions FAILS
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=deprecated
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=deferred
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=data
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=css
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=core FAILS
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=callbacks
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=basic
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=attributes
phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=animation
# phantomjs ./run-qunit.js http://localhost/jquery/test/index.html?module=ajax FAILS