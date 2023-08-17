document.addEventListener("DOMContentLoaded", function() {
    let depthReached = 0;

    window.addEventListener('scroll', function() {
	let s = window.scrollY,
	    d = document.documentElement.scrollHeight,
	    c = window.innerHeight,
	    scrollPercent = (s / (d - c)) * 100;

	if (scrollPercent > depthReached) {
	    depthReached = scrollPercent;
	}

	if (depthReached > 50 && depthReached < 55) {
	    console.log("Der Benutzer hat 50% der Seite erreicht");
	} else if (depthReached > 80 && depthReached < 85) {
	    console.log("Der Benutzer hat 80% der Seite erreicht");
	}
    });
});
