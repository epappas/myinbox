'use strict';

var express = require('express');
var router = express.Router();
var departments = require('../controllers/departments');
var stats = require('../controllers/stats');

module.exports = function(app) {
    router.route('/stats')
        .get(stats.get)

    // REGISTER ROUTE 
    // =========================================================================
    app.use('/api/v1/', router);
};