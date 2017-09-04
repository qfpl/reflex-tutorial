
var button = document.getElementById("rxjs-button");

var clicks = Rx.Observable.fromEvent(button, "click");

var ones = clicks.scan(count => count + 1, 0);

var hundreds = ones.map(function(x) { return x * 100; });

var sum = ones.combineLatest(hundreds, function(o, h) {
            return o + h; 
          });

sum.subscribe(function(s) { alert(s); });
