// unselect group buttons
$("input:radio[name='component']").click(function() {
  //var selected = $("input:radio[name='component']:checked").val();
  //var current_val = $('#selected_text').val();
  //$('#selected_text').val(current_val + selected);
  $("input:radio[name='component']").prop('checked', false);  
});

// put cursor at the end
  
  
  





shinyjs.get_replacement_location = function() {
  //uses jquery syntax to find the textArea 'phrase' & get selection properties
  Shiny.setInputValue(
    'selected_text', [
      $('#phrase').prop('selectionStart'),
      $('#phrase').prop('selectionEnd')
    ]
  );
};


shinyjs.init = function() {
  //$('#components').on('click', function() {
    //get_replacement_location;
    //$('#components').prop('checked', false);
  //);
  $('#phrase').mouseup(get_replacement_location);
  $('#phrase').keyup(get_replacement_location);
};
