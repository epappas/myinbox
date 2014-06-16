'use strict';

var fs = require('fs');
var path = require('path');
var express = require('express');
var passport = require('passport');
var router = express.Router();
var controller = require('../controllers/index');

module.exports = function(app, redis) {

    router.route('/facebook')
        .get(passport.authenticate('facebook', {
            scope: ['email', 'user_location']
        }))

    router.route('/facebook/callback')
        .get(passport.authenticate('facebook', {
            scope: ['email', 'user_location']
        }), function(req, res) {
            res.redirect(req.session.returnTo || '/');
        })

    router.route('/google')
        .get(passport.authenticate('google', {
            scope: 'profile email'
        }))

    router.route('/google/callback')
        .get(passport.authenticate('google', {
            failureRedirect: '/login'
        }), function(req, res) {
            res.redirect(req.session.returnTo || '/');
        })

    router.route('/twitter')
        .get(passport.authenticate('twitter'))

    router.route('/twitter/callback')
        .get(passport.authenticate('twitter', {
            failureRedirect: '/login'
        }), function(req, res) {
            res.redirect(req.session.returnTo || '/');
        })

    // REGISTER ROUTE 
    // =========================================================================
    app.use('/auth', router);
};