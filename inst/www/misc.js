window.onbeforeunload = function() { return true; }


$(function() { 
	$(document).keyup(function(e) {
 		if ((e.ctrlKey || e.metaKey) && (e.which==80 || e.which==77 || e.which==71 || e.which==68 || e.which==72 || e.which==86 || e.which==84 || e.which==67)) {
			Shiny.onInputChange('keyboard_pressed', String.fromCharCode(e.which));
		}
	});

	$(document).on('click', '.help-text', function() {
		alert($(this).html() + '\n\n' + $(this).attr('title'));
	});
})


$(document).on('shiny:disconnected', function(event) {
	$('.not-running').show()
})