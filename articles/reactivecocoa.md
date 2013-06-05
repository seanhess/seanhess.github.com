Reactive Cocoa
==============

### Start with some code

    // User.h

    @protocol UserDelegate <NSObject>
    -(void)userDidLogin;
    -(void)userDidLogout;
    @end

    @interface User : NSObject
    @property (weak, nonatomic) id<UserDelegate>delegate;
    @property (nonatomic) BOOL loggedIn;
    @end


    // TestViewController.m

    @interface TestViewController () <UserDelegate>
    @property (nonatomic, strong) User * user;
    @property (nonatomic, strong) UIButton * logoutButton;
    @property (nonatomic, strong) UIButton * loginButton;
    @end

    @implementation TestViewController

    - (void)viewDidLoad
    {
        [super viewDidLoad];

        // 1. DELEGATES
        // problems:
        // - only one delegate allowed
        // - I don't actually "own" user
        // - events are a strange way to do this. What if there is more state?
        // - has user even been set yet??
        self.user.delegate = self;
        
        
        
        // 2. NOTIFICATIONS (basically the same
        // - same problems
        // - (+) now multiple listeners
        // - but this funky global notification center.
        // - have to remember to remove observer!
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDidLogin) name:@"login" object:self.user];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDidLogut) name:@"login" object:self.user];
        
        
        
        // 3. KVO
        // - data driven
        // - error prone: "loggedIn" string
        // - bad syntax: in 3 places. Big if/else statement
        // - have to remember to remove observer!
        [self.user addObserver:self forKeyPath:@"loggedIn" options:NSKeyValueObservingOptionNew context:nil];
        
        
        
        
        // 4. ReactiveCocoa
        // - user doesn't even have to be set!!
        // - great code locality
        // - don't have to use the string name of the property
        // - LOTS more great features
        [RACAble(self.user.loggedIn) subscribeNext:^(id x) {
            [self setButtonsBasedOnLogin];
        }];



        // 5. Even better???
    }

    // 1/2. DELEGATES and NOTIFICATIONS
    - (void)userDidLogin {
        self.logoutButton.hidden = NO;
        self.loginButton.hidden = YES;
    }

    - (void)userDidLogout {
        self.logoutButton.hidden = YES;
        self.loginButton.hidden = NO;
    }

    // 3. KVO
    -(void)observeValueForKeyPath:(NSString *)keyPath ofObject:(id)object change:(NSDictionary *)change context:(void *)context {
        if ([keyPath isEqualToString:@"loggedIn"] && object == self.user) {
            [self setButtonsBasedOnLogin];
        }
    }    

    // 3/4. IMPROVED data-driven
    - (void)setButtonsBasedOnLogin {
        self.logoutButton.hidden = self.user.loggedIn;
        self.loginButton.hidden = !self.user.loggedIn;
    }

    @end


### What what is it?

> "OK, so it’s like KVO, plus Cocoa Bindings, except not sucking" 
> Alex Curylo

Yeah, but a lot more.


### What is Reactive Programming?

* Like a spreadsheet.

    A = B + C

* Programs are: "outputs" as the sum of all inputs over time. 

* Common semantics for UI Events, network callbacks, data changes

### Reactive Cocoa

https://github.com/blog/1107-reactivecocoa-for-a-better-world

### Full Example

https://github.com/joshaber/RACSignupDemo

### Great Reading

* https://github.com/ReactiveCocoa/ReactiveCocoa
* http://stackoverflow.com/a/1028642/105678
* http://blog.maybeapps.com/post/42894317939/input-and-output
* https://speakerdeck.com/joshaber/better-code-for-a-better-world

