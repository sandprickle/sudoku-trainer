const colors = require("tailwindcss/colors");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.elm", "./public/**/*.html"],
  theme: {
    extend: {

      gridTemplateColumns: {
        layout: '1fr',
      },

      gridTemplateRows: {
        layout: 'auto 1fr',
      },

      colors: {
        primary: colors.rose[700],
      },


    },
  },
  plugins: [],
}
