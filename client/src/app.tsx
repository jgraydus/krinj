import { BrowserRouter, Link, Route, Routes } from 'react-router-dom'

import RootPage from './root-page'

import { doStuff } from './test'

const Issues = () =>
  <div>
    <div>Issues</div>
    <Link to="/">home</Link>
  </div>

const Home = () =>
  <div>
    <div>Hello, World!</div>
    <button onClick={doStuff}>click me</button>
    <Link to="/issues">issues</Link>
  </div>

const Projects = () =>
  <div>
    <div>Projects!</div>
    <Link to="/">home</Link>
  </div>

export default () =>
  <RootPage>
    <BrowserRouter>
       <Routes>
         <Route path="/" element={<Home />} />
         <Route path="projects" elemnt={<Projects />} />
         <Route path="issues" element={<Issues />} />
       </Routes>
    </BrowserRouter>
  </RootPage>

