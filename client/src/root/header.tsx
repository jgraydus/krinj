import { ReactNode } from 'react'
import { NavLink } from 'react-router-dom'
import styled from 'styled-components'

import Spacer from '../components/spacer'

const Root = styled.div`
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  a { text-decoration: none; }
`
const Title = styled.div`
  font-size: 24px;
  font-style: bold; 
`
const NavItem = styled.div`
  padding: 5px;
  color: black;
`
const Link = ({ to, children }: { to: string, children: ReactNode }) =>
  <NavLink to={to} style={({ isActive }) => ({ color: isActive ? 'red' : 'black' })}>
    <NavItem>{children}</NavItem>
  </NavLink>

export default () =>
  <Root>
    <Link to="/projects">
      <Title>Krinj</Title>
    </Link>
  </Root>

