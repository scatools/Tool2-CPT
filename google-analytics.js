
  
  (function(i,s,o,g,r,a,m){
  i['GoogleAnalyticsObject']=r;
  i[r]=i[r] || 
  function(){
    (i[r].q=i[r].q||[]).push(arguments);
  },i[r].l=1*new Date();
  a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;
  m.parentNode.insertBefore(a,m);
})(window,document,'script',
  'https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-142234615-10', 'auto');
ga('send', 'pageview');

 
 $(document).on('change', 'select', function(e) {
    ga('send', 'event', 'widget', 'select data', $(e.currentTarget).val());
});

$(document).on('click', 'button', function(e) {
  ga('send', 'event', 'button', 'button click', e.currentTarget);
});




$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'oneprojectmode') {
    ga('send', 'event', 'mode', 'single project');
  }

});

$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'multipleprojectmode') {
    ga('send', 'event', 'mode', 'multiple project');
  }
  
});