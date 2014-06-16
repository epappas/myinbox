'use strict';

module.exports = function(redis) {
    return {
        renderIndex: function(req, res) {
            res.render('index', { title: 'Express' });
        },
        renderPartials: function(req, res) {
            var name = req.params.name;
            res.render('partials/' + name);
        }
    };
};
