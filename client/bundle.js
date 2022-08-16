var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
define("index", ["require", "exports", "react/jsx-runtime", "react-dom"], function (require, exports, jsx_runtime_1, react_dom_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    react_dom_1 = __importDefault(react_dom_1);
    const App = () => (0, jsx_runtime_1.jsx)("div", { children: "Hello, World!" });
    react_dom_1.default.render((0, jsx_runtime_1.jsx)(App, {}), document.getElementById('root'));
});
