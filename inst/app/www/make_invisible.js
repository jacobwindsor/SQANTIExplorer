$( document ).ready(function() {
  Shiny.addCustomMessageHandler('make_invisible', function(id) {
    $("#" + id).css({ visibility: "hidden" });
  });
});
