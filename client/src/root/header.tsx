import { NavLink } from 'react-router-dom'
import styled from 'styled-components'

import Spacer from '../components/spacer'

const Root = styled.div`
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
`
const Title = styled.div`
  font-size: 24px;
  font-style: bold; 
`
const Nav = styled.nav`
  display: flex;
  flex-flow: row nowrap;
`
const NavItem = styled.div`
  padding: 5px;
  text-decoration: none;
`
const Link = ({ to, children }) =>
  <NavLink to={to} style={({ isActive }) => ({ color: isActive ? 'red' : 'black' })}>
    <NavItem>{children}</NavItem>
  </NavLink>

export default () =>
  <Root>
    <Title>Issue Tracker</Title>
    <Spacer width={10} />
    <Nav>
      <Link to="/projects">Projects</Link>
      <Link to="/issues">Issues</Link>
    </Nav>
  </Root>

