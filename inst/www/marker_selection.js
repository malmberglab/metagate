/*

  MetaGate: R package for visualization and statistical analysis of cytometry data.
  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
  
*/

$(document).on('change', '.marker_selection_included_column input', function() {
	if ($(this).is(':checked')) {
		$(this).parents('.marker_selection_body').removeClass('excluded')
	} else {
		$(this).parents('.marker_selection_body').addClass('excluded')
	}
})


function marker_selection_apply(row) {
	var transform = $('#import_marker_selection_transform_' + row).val();
	var cofactor = $('#import_marker_selection_cofactor_' + row).val()
	$('.marker_selection_transform select').val(transform)
	$('.marker_selection_cofactor input').val(cofactor)
	if (transform == 'arcsinh') {
		$('.marker_selection_cofactor input').show()
	} else {
		$('.marker_selection_cofactor input').hide()
	}

	var markers = $('.marker_selection_body').length

	for (marker = 1; marker <= markers; marker++) {
		Shiny.onInputChange('import_marker_selection_transform_' + marker, transform)
		Shiny.onInputChange('import_marker_selection_cofactor_' + marker, cofactor)
	}
}

$(document).on('change', '.marker_selection_transform select', function() {
	const cofactorInput = $('#' + $(this).attr('id').replace('transform', 'cofactor'));
	if ($(this).val() == "arcsinh") {
		cofactorInput.show();
	} else {
		cofactorInput.hide();
	}
})