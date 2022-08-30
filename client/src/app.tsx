import { Link, Navigate, Route, Routes } from 'react-router-dom'

import { Project, Issues } from './projects/project'
import Projects from './projects'
import Root from './root'

export default () =>
  <Root>
     <Routes>
       <Route path="/" element={<Navigate to="/projects" />} />
       <Route path="/projects">
         <Route path="" element={<Projects />} />
         <Route path=":projectId">
           <Route path="" element={<Project />} />
           <Route path="issues" element={<Issues />} />
         </Route>
       </Route>
     </Routes>
  </Root>

