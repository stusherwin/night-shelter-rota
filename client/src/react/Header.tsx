import * as React from 'react';
import { Vol } from './Types'
import { MessageBubble, MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { ApiError } from './ServerApi'

export type VolDetailsState = 'NotEditing' 
                            | 'EditingNewVol'
                            | 'EditingCurrentVol'

export interface HeaderProps { reqInProgress: boolean
                             , initialDataLoaded: boolean
                             , vols: Vol[]
                             , error: ApiError | null
                             , changeCurrentVol: (vol: Vol | null) => void
                             , editCurrentVol: () => void
                             , editNewVol: () => void
                             }

export interface HeaderState { volDetailsState: VolDetailsState
                             , currentVol: Vol | null
                             , errorMessage: MessageBubbleProps
                             }

export class Header extends React.Component<HeaderProps, HeaderState> {
  constructor(props: HeaderProps) {
    super(props)
    this.state = { volDetailsState: 'NotEditing'
                 , currentVol: null
                 , errorMessage: new MessageBubbleProps()
                 }
  }

  componentWillReceiveProps(props: HeaderProps) {
    if(props.error != this.props.error) {
      this.setState(
        { errorMessage: props.error
                        ? this.state.errorMessage.setMessage(
                            { header: props.error.error
                            , body: props.error.message
                            , position: 'under'
                            , icon: 'warning'
                            })
                        : this.state.errorMessage.clear()
        })
    }
  }

  render() {
    if(!this.props.initialDataLoaded) {
      return (
        <div className="header initial-data-loading">
          <StatusIcon reqInProgress={this.props.reqInProgress}
                      errorMessage={this.state.errorMessage}
                      action={this.errorMessageAction.bind(this)}>
          </StatusIcon>
          <h2>Night Shelter Rota</h2>
        </div>
      )
    } else {
      return (
        <div className="header">
          <HeaderButtons volDetailsState={this.state.volDetailsState}
                         currentVol={this.state.currentVol}
                         editNewVol={this.editNewVol.bind(this)}
                         editCurrentVol={this.editCurrentVol.bind(this)} />
          <StatusIcon reqInProgress={this.props.reqInProgress}
                      errorMessage={this.state.errorMessage}
                      action={this.errorMessageAction.bind(this)}>
          </StatusIcon>
          <h2>Night Shelter Rota for </h2>
          <select className="vol-select"
                  onChange={e => { this.changeCurrentVol.bind(this)(e.target.value == ''
                                                                 ? null
                                                                 : this.props.vols.find(v => v.id == parseInt(e.target.value)) || null) }}>
            <option value="">All volunteers</option>
            {this.props.vols.sort((a, b) => a.name.localeCompare(b.name))
                            .map(v =>
              <option selected={this.state.currentVol != null && this.state.currentVol.id == v.id}
                      value={v.id}>{v.name}
              </option>)}
          </select>
        </div>
      )
    }
  }

  errorMessageAction(action: MessageBubbleAction) {
    this.setState({ errorMessage: this.state.errorMessage.afterAction(action)
                  })
  }

  changeCurrentVol(vol: Vol | null) {
    console.log('current vol: ' + (vol? vol.name : 'none'))
    this.setState({ currentVol: vol
                  , volDetailsState: 'NotEditing'
                  , errorMessage: this.state.errorMessage.clear()
                  })
    this.props.changeCurrentVol(vol)
  }

  editNewVol() {
    console.log('edit new vol')
    this.setState({ volDetailsState: 'EditingNewVol'
                  , errorMessage: this.state.errorMessage.clear()
                  })
    this.props.editNewVol()
  }

  editCurrentVol() {
    console.log('edit current vol')
    this.setState({ volDetailsState: 'EditingCurrentVol'
                  , errorMessage: this.state.errorMessage.clear()
                  })
    this.props.editCurrentVol()
  }
}

function HeaderButtons(props: { volDetailsState: VolDetailsState 
                              , currentVol: Vol | null
                              , editNewVol: () => void
                              , editCurrentVol: () => void
                              }): JSX.Element {
  let buttons = [];
  if(props.volDetailsState == 'NotEditing') {
    buttons.push(<HeaderButton buttonClassName="header-button-new"
                               mediaClassName="media-large-screen"
                               text="New volunteer"
                               icon="add"
                               action={props.editNewVol} />)
    buttons.push(<HeaderButton buttonClassName="header-button-new"
                               mediaClassName="media-larger-screen media-medium-screen"
                               text="New"
                               icon="add"
                               action={props.editNewVol} />)
    buttons.push(<HeaderButton buttonClassName="mini icon header-button-new"
                               mediaClassName="media-small-screen"
                               text={null}
                               icon="add"
                               action={props.editNewVol} />)

    if(props.currentVol != null) {
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-large-screen"
                                 text="Edit volunteer details"
                                 icon="edit"
                                 action={props.editCurrentVol} />)
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-larger-screen media-medium-screen"
                                 text="Edit"
                                 icon="edit"
                                 action={props.editCurrentVol} />)
      buttons.push(<HeaderButton buttonClassName="mini icon header-button-edit"
                                 mediaClassName="media-small-screen"
                                 text={null}
                                 icon="edit"
                                 action={props.editCurrentVol} />)
    }
  }
  
  return <div className="header-buttons">
           {buttons}
         </div>
}

function HeaderButton(props: { buttonClassName: string
                             , mediaClassName: string
                             , text: string | null
                             , icon: string
                             , action: () => void
                             } ): JSX.Element | null {
  return <button className={`ui button header-button ${props.buttonClassName} ${props.mediaClassName}`}
                 onClick={e => {e.preventDefault(); props.action(); }}>
           <i className={`icon icon-${props.icon}`}></i>
           {props.text}
         </button>
}

class StatusIcon extends React.Component<{ reqInProgress: boolean
                                         , errorMessage: MessageBubbleProps
                                         , action: (action: MessageBubbleAction) => void
                                         }, {}> {
  render() {
    return <div className="header-status">
             <i className={this.iconType()}
                onClick={e => { e.preventDefault(); this.props.action('ToggleFixed'); }}
                onMouseOver={e => { e.preventDefault(); this.props.action('ShowTransitory'); }}
                onMouseLeave={e => { e.preventDefault(); this.props.action('HideTransitory'); }}>
             </i>
             <MessageBubble {...this.props.errorMessage}
                            action={this.props.action}>
             </MessageBubble>
          </div>
  }

  iconType() {
    if(this.props.reqInProgress) {
      return 'icon-spin animate-spin'
    }

    return this.props.errorMessage.message
             ? 'icon-warning'
             : 'logo'
  }
}
