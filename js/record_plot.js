(function($) {
    $(document).ready(function() {
	
	$('#record_plot').scianimator({
	    'images': [],
	    'width': 480,
	    'delay': 100,
	    'loopMode': 'loop'
	});
	$('#record_plot').scianimator('play');
    });
})(jQuery);
