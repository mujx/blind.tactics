"use strict";

let Chartist = require("chartist");

exports.mkLineChart = function (selector) {
  return function (series) {
    return function () {
      return new Chartist.Line(
        selector,
        {
          series: [series],
        },
        {
          height: "200px",
          showArea: true,
        }
      );
    };
  };
};

exports.update = function (chart) {
  return function (series) {
    return function () {
      chart.update({ series: [series] });
    };
  };
};

exports.detach = function (chart) {
  return function () {
    chart.detach();
  };
};
