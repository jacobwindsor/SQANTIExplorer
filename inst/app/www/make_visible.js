$( document ).ready(function() {
  Shiny.addCustomMessageHandler('make_visible', function(id) {
    $("#" + id).css({ visibility: "visible" });
  });
});
