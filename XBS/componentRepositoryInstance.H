:- export services/2,          % services(Interface,Service)
          inputParameters/2,   % inputParameters(Service, Parameter)
          outParameters/2,     % outputParameters(Service, Parameter)
          typeOf/2,            % typeOf(Parameter, DataType)
          provides/2,          % provides(Component, Interface)
          requires/2,          % requires(Component, Interface)
          subComponents/2,     % subComponents(Component, Component)
          assembledTo/4,       % assembledTo(Component,  the composite component
                               %             Component,  a sub component
                               %             Interface,  a required Interface by the sub component
                               %             Component)  another sub component providing that interface
          delegateProvides/3,  % delegateProvides(Component,  the composite component
                               %                  Interface,  an Interface provided by the composite component
                               %                  Component)  the sub component actually providing it
          delegateRequires/3.  % delegateRequires(Component,  the composite component
                               %                  Interface,  an Interface required by the composite component
                               %                  Component)  the sub components actually requiring it
